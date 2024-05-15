use std::{
    fmt::Display,
    path::{Path, PathBuf},
};

use crate::{
    error::ParseError,
    parse_path,
    types::{Declaration, DeclarationKind, ExternTaskParams, Program},
};

#[derive(Debug, thiserror::Error)]
pub struct ModuleError {
    pub mod_path: PathBuf,
    #[source]
    pub kind: ModuleErrorKind,
}

impl Display for ModuleError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{:?}", self)
    }
}

impl ModuleError {
    pub fn path_does_not_exist(mod_path: PathBuf) -> Self {
        Self {
            mod_path,
            kind: ModuleErrorKind::PathDoesNotExist,
        }
    }
}

#[derive(Debug, thiserror::Error)]
pub enum ModuleErrorKind {
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
    pub fn dependency_exports(&self) -> Vec<Declaration> {
        let mut exports = vec![];
        for dep in &self.dependencies {
            exports.extend(dep.exports());
        }
        exports
    }

    pub fn exports(&self) -> Vec<Declaration> {
        let mut exports = vec![];
        for decl in &self.program.declarations {
            let kind = &decl.kind;
            let span = decl.span.clone();
            match kind {
                DeclarationKind::ExternTask {
                    name,
                    params,
                    returns,
                } => exports.push(
                    DeclarationKind::ExternTask {
                        name: name.clone(),
                        params: params.clone(),
                        returns: returns.clone(),
                    }
                    .with_span(span),
                ),
                DeclarationKind::Task {
                    name,
                    params,
                    returns,
                    ..
                } => exports.push(
                    DeclarationKind::ExternTask {
                        name: name.clone(),
                        params: ExternTaskParams::WellKnown(params.clone()),
                        returns: returns.clone(),
                    }
                    .with_span(span),
                ),
                DeclarationKind::Struct { name, members } => exports.push(
                    DeclarationKind::Struct {
                        name: name.clone(),
                        members: members.clone(),
                    }
                    .with_span(span),
                ),
                DeclarationKind::Enum { name, variants } => exports.push(
                    DeclarationKind::Enum {
                        name: name.clone(),
                        variants: variants.clone(),
                    }
                    .with_span(span),
                ),
                DeclarationKind::Global { name, type_, .. } => exports.push(
                    DeclarationKind::ExternGlobal {
                        name: name.clone(),
                        type_: type_.clone(),
                    }
                    .with_span(span),
                ),
                DeclarationKind::ExternGlobal { name, type_ } => exports.push(
                    DeclarationKind::ExternGlobal {
                        name: name.clone(),
                        type_: type_.clone(),
                    }
                    .with_span(span),
                ),
                DeclarationKind::Directive { .. } | DeclarationKind::Use { .. } => {}
            }
        }
        exports
    }
}

pub fn parse_module_hierarchy<P: AsRef<Path>>(
    main_file: P,
    search_paths: &[&str],
) -> Result<(TblModule, Vec<ModuleError>), ModuleError> {
    let name = main_file
        .as_ref()
        .file_stem()
        .ok_or(ModuleError::path_does_not_exist(
            main_file.as_ref().to_owned(),
        ))?;
    let mut errors = vec![];
    let (program, errs) = parse_path(&main_file);
    errors.extend(errs.iter().map(|e| ModuleError {
        mod_path: main_file.as_ref().to_path_buf(),
        kind: ModuleErrorKind::ParseError(e.clone()),
    }));
    // errors.append(&mut errs);
    let mut dependencies = vec![];
    for decl in &program.declarations {
        if let DeclarationKind::Use { module } = &decl.kind {
            let paths = search_paths.iter().map(|p| format!("{p}/{module}.tbl"));
            for p in paths {
                if std::fs::File::open(&p).is_ok() {
                    let (submodule, mut submodule_errors) =
                        parse_module_hierarchy(&p, search_paths)?;
                    dependencies.push(submodule);
                    errors.append(&mut submodule_errors);
                    break;
                }
            }
        }
    }
    Ok((
        TblModule {
            name: name.to_string_lossy().to_string(),
            program,
            dependencies,
        },
        errors,
    ))
}
