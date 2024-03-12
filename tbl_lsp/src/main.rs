use tbl_parser::Source;
use tower_lsp::jsonrpc::Result;
use tower_lsp::lsp_types::{
    CompletionItem, CompletionOptions, CompletionParams, CompletionResponse, Diagnostic,
    DiagnosticOptions, DiagnosticServerCapabilities, DocumentDiagnosticParams,
    DocumentDiagnosticReport, DocumentDiagnosticReportPartialResult,
    DocumentDiagnosticReportResult, FullDocumentDiagnosticReport, Hover, HoverContents,
    HoverParams, HoverProviderCapability, InitializeResult, InitializedParams, MarkedString,
    MessageType, Position, Range, RelatedFullDocumentDiagnosticReport, ServerCapabilities,
    ServerInfo, WorkDoneProgressOptions,
};
use tower_lsp::LspService;
use tower_lsp::{lsp_types::InitializeParams, Client, LanguageServer, Server};

struct Backend {
    client: Client,
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
                        workspace_diagnostics: false,
                        work_done_progress_options: WorkDoneProgressOptions {
                            work_done_progress: None,
                        },
                    },
                )),
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

    async fn completion(&self, _: CompletionParams) -> Result<Option<CompletionResponse>> {
        Ok(Some(CompletionResponse::Array(vec![
            CompletionItem::new_simple("Hello".to_string(), "Some detail".to_string()),
            CompletionItem::new_simple("Bye".to_string(), "More detail".to_string()),
        ])))
    }

    async fn hover(&self, _: HoverParams) -> Result<Option<Hover>> {
        Ok(Some(Hover {
            contents: HoverContents::Scalar(MarkedString::String("You're hovering!".to_string())),
            range: None,
        }))
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

        let (_, errors) = tbl_parser::parse(path);

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

    let (service, socket) = LspService::new(|client| Backend { client });
    Server::new(stdin, stdout, socket).serve(service).await;
}
