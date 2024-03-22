use std::collections::HashSet;

use tbl_parser::{
    module::TblModule,
    types::{DeclarationKind, Program},
    Span,
};

pub struct Diagnostic {
    pub span: Span,
    pub level: DiagnosticLevel,
    pub message: String,
}

pub enum DiagnosticLevel {
    Info,
    Warning,
    Error,
}

pub struct TblAnalyzer {
    program: TblModule,
}

impl TblAnalyzer {
    pub fn new(program: TblModule) -> Self {
        Self { program }
    }

    pub fn analyze(&self) -> Vec<Diagnostic> {
        let mut diagnostics = vec![];
        diagnostics.extend(self.unused_globals());
        diagnostics.extend(self.unknown_vars());
        diagnostics
    }

    fn unused_globals(&self) -> Vec<Diagnostic> {
        let mut diagnostics = vec![];
        let mut globals = HashSet::new();
        let mut referenced_vars = HashSet::new();
        let program = &self.program.program;
        for decl in &program.declarations {
            let kind = &decl.kind;
            let span = decl.span.clone();
            if let DeclarationKind::Global { name, .. } = kind {
                globals.insert((name, span));
            }
            if let DeclarationKind::Task { body, .. } = kind {
                for stmt in body {
                    referenced_vars.extend(stmt.referenced_vars());
                }
            }
        }
        let referenced_vars: Vec<_> = referenced_vars.iter().map(|(var, _)| *var).collect();
        for (glob, span) in globals.iter() {
            if !referenced_vars.contains(&glob.as_str()) {
                diagnostics.push(Diagnostic {
                    span: span.clone(),
                    level: DiagnosticLevel::Info,
                    message: format!("Global `{glob}` is never used"),
                })
            }
        }
        diagnostics
    }

    fn unknown_vars(&self) -> Vec<Diagnostic> {
        let mut globals = vec![];
        let mut diagnostics = vec![];
        let exported_vars: Vec<_> = self
            .program
            .dependency_exports()
            .iter()
            .filter_map(|d| match &d.kind {
                DeclarationKind::ExternTask { name, .. } => Some(name.to_string()),
                DeclarationKind::Task { name, .. } => Some(name.to_string()),
                DeclarationKind::Struct { .. } => None,
                DeclarationKind::Global { name, .. } => Some(name.to_string()),
                DeclarationKind::Directive { .. } | DeclarationKind::Use { .. } => None,
                DeclarationKind::ExternGlobal { name, .. } => Some(name.to_string()),
            })
            .collect();
        let program = &self.program.program;
        for decl in &program.declarations {
            if let DeclarationKind::Global { name, .. } = &decl.kind {
                globals.push(name.as_str());
            }
            if let DeclarationKind::Task {
                params,
                locals,
                body,
                ..
            } = &decl.kind
            {
                let mut vars: Vec<_> = params.iter().map(|(p, _)| p.as_str()).collect();
                vars.extend(locals.iter().map(|(l, _)| l.as_str()));
                for stmt in body {
                    for (var, span) in stmt.referenced_vars() {
                        if !(vars.contains(&var)
                            || globals.contains(&var)
                            || exported_vars.contains(&var.to_string()))
                        {
                            diagnostics.push(Diagnostic {
                                span: span.clone(),
                                level: DiagnosticLevel::Error,
                                message: format!("Unknown variable `{var}`"),
                            })
                        }
                    }
                }
            }
        }
        diagnostics
    }
}
