use std::{
    fmt::Display,
    path::{Path, PathBuf},
};

use crate::{
    error::ParseError,
    parse_path,
    types::{self, Declaration, DeclarationKind, ExternTaskParams, Program},
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
    pub fn lsp_new(program: Program) -> Self {
        TblModule {
            name: String::new(),
            program,
            dependencies: vec![],
        }
    }

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
                    path,
                    params,
                    returns,
                } => exports.push(
                    DeclarationKind::ExternTask {
                        path: path.clone(),
                        params: params.clone(),
                        returns: returns.clone(),
                    }
                    .with_span(span),
                ),
                DeclarationKind::Task {
                    path,
                    params,
                    returns,
                    ..
                } => exports.push(
                    DeclarationKind::ExternTask {
                        path: path.prefixed(self.name.clone()),
                        params: ExternTaskParams::WellKnown(params.clone()),
                        returns: returns.clone(),
                    }
                    .with_span(span),
                ),
                DeclarationKind::Struct { path, members } => exports.push(
                    DeclarationKind::Struct {
                        path: path.prefixed(self.name.clone()),
                        members: members.clone(),
                    }
                    .with_span(span),
                ),
                DeclarationKind::Enum { path, variants } => exports.push(
                    DeclarationKind::Enum {
                        path: path.prefixed(self.name.clone()),
                        variants: variants.clone(),
                    }
                    .with_span(span),
                ),
                DeclarationKind::Global { path, type_, .. } => exports.push(
                    DeclarationKind::ExternGlobal {
                        path: path.prefixed(self.name.clone()),
                        type_: type_.clone(),
                    }
                    .with_span(span),
                ),
                DeclarationKind::ExternGlobal { path, type_ } => exports.push(
                    DeclarationKind::ExternGlobal {
                        path: path.prefixed(self.name.clone()),
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

pub fn parse_module_hierarchy<P: AsRef<Path>, Q: Into<types::Path> + Clone>(
    main_file: P,
    search_paths: &[&str],
    prefix: Q,
) -> Result<(TblModule, Vec<ModuleError>), ModuleError> {
    let raw_name = main_file
        .as_ref()
        .file_stem()
        .ok_or(ModuleError::path_does_not_exist(
            main_file.as_ref().to_owned(),
        ))?;
    let name = raw_name.to_string_lossy().to_string();
    let mut errors = vec![];
    let (program, errs) = parse_path(&main_file, prefix.clone());
    errors.extend(errs.iter().map(|e| ModuleError {
        mod_path: main_file.as_ref().to_path_buf(),
        kind: ModuleErrorKind::ParseError(e.clone()),
    }));
    // errors.append(&mut errs);
    let name_path = types::Path::from_ident(&name);
    let mut dependencies = vec![];
    for decl in &program.declarations {
        if let DeclarationKind::Use { module } = &decl.kind {
            let paths = search_paths.iter().map(|p| format!("{p}/{module}.tbl"));
            for p in paths {
                if std::fs::File::open(&p).is_ok() {
                    let (submodule, mut submodule_errors) = parse_module_hierarchy(
                        &p,
                        search_paths,
                        name_path.prefixed(prefix.clone()),
                    )?;
                    dependencies.push(submodule);
                    errors.append(&mut submodule_errors);
                    break;
                }
            }
        }
    }
    Ok((
        TblModule {
            name,
            program,
            dependencies,
        },
        errors,
    ))
}
