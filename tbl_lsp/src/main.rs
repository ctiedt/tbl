use std::collections::HashMap;
use std::sync::Arc;

use tbl_analysis::{DiagnosticLevel, TblAnalyzer};
use tbl_parser::module::{parse_module_hierarchy, TblModule};
use tbl_parser::types::{self, DeclarationKind, Program};
use tbl_parser::Source;
use tokio::sync::RwLock;
use tower_lsp::jsonrpc::Result;
use tower_lsp::lsp_types::{
    self, CompletionItem, CompletionOptions, CompletionParams, CompletionResponse, Diagnostic,
    DiagnosticOptions, DiagnosticServerCapabilities, DiagnosticSeverity,
    DidChangeTextDocumentParams, DidCloseTextDocumentParams, DidOpenTextDocumentParams,
    DidSaveTextDocumentParams, DocumentDiagnosticParams, DocumentDiagnosticReport,
    DocumentDiagnosticReportResult, FullDocumentDiagnosticReport, Hover, HoverContents,
    HoverParams, HoverProviderCapability, InitializeResult, InitializedParams, MarkedString,
    MessageType, OneOf, Position, Range, RelatedFullDocumentDiagnosticReport, ServerCapabilities,
    ServerInfo, TextDocumentSyncKind, Url, WorkDoneProgressOptions,
};
use tower_lsp::LspService;
use tower_lsp::{lsp_types::InitializeParams, Client, LanguageServer, Server};

struct Backend {
    client: Client,
    docs: Arc<RwLock<HashMap<Url, String>>>,
}

impl Backend {
    fn validate(&self, source: Source) -> Vec<Diagnostic> {
        let mut diagnostics = vec![];
        let (prog, errors) = tbl_parser::parse(source, tbl_parser::types::Path::default());
        // let (prog, _) = match parse_module_hierarchy(path, &[".", "lib"]) {
        //     Ok(prog) => prog,
        //     Err(e) => {
        //         diagnostics.push(Diagnostic::new_simple(Range::default(), format!("{e:?}")));
        //         return diagnostics;
        //     }
        // };
        if !errors.is_empty() {
            return errors
                .into_iter()
                .map(|e| Diagnostic::new_simple(Range::default(), format!("{e:?}")))
                .collect();
        }
        //self.program = prog;

        let analyzer = TblAnalyzer::new(TblModule::lsp_new(prog));

        for diag in analyzer.analyze() {
            let (start, end) = source.row_col(diag.span.clone());
            let range = Range::new(
                Position::new(start.line as u32, start.column as u32),
                Position::new(end.line as u32, end.column as u32),
            );
            let severity = match diag.level {
                DiagnosticLevel::Info => DiagnosticSeverity::INFORMATION,
                DiagnosticLevel::Warning => DiagnosticSeverity::WARNING,
                DiagnosticLevel::Error => DiagnosticSeverity::ERROR,
            };
            diagnostics.push(Diagnostic::new(
                range,
                Some(severity),
                None,
                None,
                diag.message,
                None,
                None,
            ));
        }

        for error in errors {
            let (start, end) = source.row_col(error.span.clone());
            let range = Range::new(
                Position::new(start.line as u32, start.column as u32),
                Position::new(end.line as u32, end.column as u32),
            );
            let message = format!("{:?}", error);
            diagnostics.push(Diagnostic::new(
                range,
                Some(DiagnosticSeverity::ERROR),
                None,
                None,
                message,
                None,
                None,
            ));
        }

        diagnostics
    }
}

#[tower_lsp::async_trait]
impl LanguageServer for Backend {
    async fn initialize(&self, _: InitializeParams) -> Result<InitializeResult> {
        Ok(InitializeResult {
            capabilities: ServerCapabilities {
                hover_provider: Some(HoverProviderCapability::Simple(true)),
                completion_provider: Some(CompletionOptions::default()),
                diagnostic_provider: Some(DiagnosticServerCapabilities::Options(
                    DiagnosticOptions {
                        identifier: Some("tbl".to_string()),
                        inter_file_dependencies: true,
                        workspace_diagnostics: true,
                        work_done_progress_options: WorkDoneProgressOptions {
                            work_done_progress: None,
                        },
                    },
                )),
                document_symbol_provider: Some(lsp_types::OneOf::Left(true)),
                text_document_sync: Some(lsp_types::TextDocumentSyncCapability::Kind(
                    TextDocumentSyncKind::FULL,
                )),
                document_highlight_provider: Some(OneOf::Left(true)),
                ..Default::default()
            },
            server_info: Some(ServerInfo {
                name: "TBL Language Server".to_string(),
                ..Default::default()
            }),
        })
    }

    async fn initialized(&self, _: InitializedParams) {
        self.client
            .log_message(MessageType::INFO, "server initialized!")
            .await;
    }

    async fn shutdown(&self) -> Result<()> {
        Ok(())
    }

    async fn did_change(&self, params: DidChangeTextDocumentParams) -> () {
        eprintln!("{:?}", params);
        let path = params.text_document.uri.path();
        let source = Source {
            name: path,
            contents: &params.content_changes[0].text,
        };
        let diagnostics = self.validate(source);
        self.client
            .publish_diagnostics(params.text_document.uri, diagnostics, None)
            .await;
        eprintln!("Changed!");
    }

    async fn did_save(&self, params: DidSaveTextDocumentParams) {
        eprintln!("{:?}", params);
        eprintln!("Saved!");
    }

    async fn completion(&self, params: CompletionParams) -> Result<Option<CompletionResponse>> {
        let path = params.text_document_position.text_document.uri.path();
        self.client
            .log_message(
                MessageType::INFO,
                format!("Received completion request {params:?}",),
            )
            .await;

        let (program, _) = tbl_parser::parse_path(path, types::Path::default());
        let mut completions = vec![
            CompletionItem::new_simple("schedule".to_string(), "schedule".to_string()),
            CompletionItem::new_simple("task".to_string(), "task".to_string()),
        ];
        for decl in program.declarations {
            let kind = &decl.kind;
            match kind {
                DeclarationKind::Task {
                    path,
                    params,
                    returns,
                    ..
                } => completions.push(CompletionItem::new_simple(
                    path.to_string(),
                    format!("task {path}()"),
                )),
                DeclarationKind::ExternTask {
                    path,
                    params,
                    returns,
                } => completions.push(CompletionItem::new_simple(
                    path.to_string(),
                    format!("task {path}()"),
                )),
                DeclarationKind::Global { path, type_, .. } => {
                    completions.push(CompletionItem::new_simple(
                        path.to_string(),
                        format!("global {path}: {}", type_.name()),
                    ))
                }
                DeclarationKind::ExternGlobal { path, type_ } => {
                    completions.push(CompletionItem::new_simple(
                        path.to_string(),
                        format!("global {path}: {}", type_.name()),
                    ))
                }
                _ => {}
            }
        }
        Ok(Some(CompletionResponse::Array(completions)))
    }

    async fn hover(&self, _: HoverParams) -> Result<Option<Hover>> {
        Ok(Some(Hover {
            contents: HoverContents::Scalar(MarkedString::String("You're hovering!".to_string())),
            range: None,
        }))
    }

    async fn did_open(&self, params: DidOpenTextDocumentParams) {
        let uri = params.text_document.uri;
        let text = params.text_document.text;
        self.docs.write().await.insert(uri, text);
    }

    async fn did_close(&self, params: DidCloseTextDocumentParams) {
        let uri = params.text_document.uri;
        self.docs.write().await.remove(&uri);
    }

    async fn diagnostic(
        &self,
        params: DocumentDiagnosticParams,
    ) -> Result<DocumentDiagnosticReportResult> {
        let path = params.text_document.uri.path();
        self.client
            .log_message(MessageType::INFO, "Received diagnostic request")
            .await;

        let contents = tokio::fs::read_to_string(path).await.unwrap();
        let source = Source {
            name: path,
            contents: &contents,
        };

        let (_, errors) = tbl_parser::parse_path(path, types::Path::default());

        let mut diagnostics = vec![];

        for error in errors {
            let (start, end) = source.row_col(error.span.clone());
            let range = Range::new(
                Position::new(start.line as u32, start.column as u32),
                Position::new(end.line as u32, end.column as u32),
            );
            let message = format!("{:?}", error);
            diagnostics.push(Diagnostic::new_simple(range, message));
        }

        Ok(DocumentDiagnosticReportResult::Report(
            DocumentDiagnosticReport::Full(RelatedFullDocumentDiagnosticReport {
                related_documents: None,
                full_document_diagnostic_report: FullDocumentDiagnosticReport {
                    result_id: None,
                    items: diagnostics,
                },
            }),
        ))
    }
}

#[tokio::main]
async fn main() {
    let stdin = tokio::io::stdin();
    let stdout = tokio::io::stdout();

    let (service, socket) = LspService::new(|client| Backend {
        client,
        docs: Arc::new(RwLock::new(HashMap::new())),
    });
    Server::new(stdin, stdout, socket).serve(service).await;
}
