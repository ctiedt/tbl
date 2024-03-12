use std::path::Path;

use tbl_parser::{
    parse,
    types::{Declaration, Program},
};

pub enum ModuleError {
    PathDoesNotExist,
    ParseError,
}

#[derive(Debug)]
pub struct TblModule {
    pub program: Program,
    pub dependencies: Vec<TblModule>,
}

pub fn parse_module_hierarchy<P: AsRef<Path>>(
    main_file: P,
    search_paths: &[&str],
) -> miette::Result<TblModule> {
    let (program, errs) = parse(main_file);
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
        program,
        dependencies,
    })
}
