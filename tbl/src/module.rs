use std::path::Path;

use tbl_parser::{
    error::ParseError,
    parse, parse_path,
    types::{Declaration, ExternTaskParams, Program},
};

#[derive(Debug, thiserror::Error)]
pub enum ModuleError {
    #[error("path does not exist")]
    PathDoesNotExist,
    #[error("parse error")]
    ParseError(#[from] ParseError),
}

#[derive(Debug)]
pub struct TblModule {
    pub name: String,
    pub program: Program,
    pub dependencies: Vec<TblModule>,
}

impl TblModule {
    pub fn exports(&self) -> Vec<Declaration> {
        let mut exports = vec![];
        for decl in &self.program.declarations {
            match decl {
                Declaration::ExternTask {
                    name,
                    params,
                    returns,
                } => exports.push(Declaration::ExternTask {
                    name: name.clone(),
                    params: params.clone(),
                    returns: returns.clone(),
                }),
                Declaration::Task {
                    name,
                    params,
                    returns,
                    ..
                } => exports.push(Declaration::ExternTask {
                    name: name.clone(),
                    params: ExternTaskParams::WellKnown(params.clone()),
                    returns: returns.clone(),
                }),
                Declaration::Struct { name, members } => exports.push(Declaration::Struct {
                    name: name.clone(),
                    members: members.clone(),
                }),
                Declaration::Global { name, type_, .. } => {
                    exports.push(Declaration::ExternGlobal {
                        name: name.clone(),
                        type_: type_.clone(),
                    })
                }
                Declaration::ExternGlobal { name, type_ } => {
                    exports.push(Declaration::ExternGlobal {
                        name: name.clone(),
                        type_: type_.clone(),
                    })
                }
                Declaration::Directive { .. } | Declaration::Use { .. } => {}
            }
        }
        exports
    }
}

pub fn parse_module_hierarchy<P: AsRef<Path>>(
    main_file: P,
    search_paths: &[&str],
) -> Result<TblModule, ModuleError> {
    let name = main_file
        .as_ref()
        .file_stem()
        .ok_or(ModuleError::PathDoesNotExist)?;
    let (program, errs) = parse_path(&main_file);
    let mut dependencies = vec![];
    for decl in &program.declarations {
        if let Declaration::Use { module } = decl {
            let paths = search_paths.iter().map(|p| format!("{p}/{module}.tbl"));
            for p in paths {
                if std::fs::File::open(&p).is_ok() {
                    let submodule = parse_module_hierarchy(&p, search_paths)?;
                    dependencies.push(submodule);
                    break;
                }
            }
        }
    }
    Ok(TblModule {
        name: name.to_string_lossy().to_string(),
        program,
        dependencies,
    })
}
