use log::{error, warn};
use once_cell::sync::Lazy;
use serde_json::Value;
use std::sync::Arc;
use tokio::sync::RwLock;
use tower_lsp::{
    jsonrpc::Result as TResult, lsp_types::*, Client, LanguageServer, LspService, Server,
};

fn get_parser() -> tree_sitter::Parser {
    let mut parser = tree_sitter::Parser::new();
    parser
        .set_language(tree_sitter_teal::language())
        .expect("Failed to load Teal language");
    parser
}

struct Backend {
    _client: Client,
    document: Arc<RwLock<Option<tree_sitter::Tree>>>,
    source: Arc<RwLock<String>>,
    url: Arc<RwLock<Option<Url>>>,
}

fn _pretty_print(source: &str, node: tree_sitter::Node, indent: usize) -> String {
    static PRINT_KINDS: Lazy<[u16; 2]> = Lazy::new(|| {
        let language = tree_sitter_teal::language();
        [
            language.id_for_node_kind("identifier", true),
            language.id_for_node_kind("simple_type", true),
        ]
    });
    let mut ret = String::new();

    if node.is_error() {
        ret.push_str("ERROR: ");
    } else if node.is_missing() {
        ret.push_str("MISSING: ");
    }
    ret.push_str(&"\t".repeat(indent));
    if PRINT_KINDS.contains(&node.kind_id()) {
        ret.push_str("\"");
        ret.push_str(node.utf8_text(source.as_bytes()).unwrap());
        ret.push_str("\"");
    } else {
        ret.push_str(node.kind());
    }
    ret.push_str("\n");

    let mut cursor = node.walk();
    for child in node.named_children(&mut cursor) {
        ret.push_str(&_pretty_print(source, child, indent + 1));
    }

    ret
}

impl Backend {
    fn collect_syntax_errors(&self, node: tree_sitter::Node, diagnostics: &mut Vec<Diagnostic>) {
        println!("Syntax check: {:?} ({})", node, node.has_error());
        let mut cursor = node.walk();
        let has_children = cursor.goto_first_child();
        let mut last_error = true;

        if has_children {
            loop {
                if cursor.node().has_error() {
                    last_error = false;
                    self.collect_syntax_errors(cursor.node(), diagnostics);
                }

                if !cursor.goto_next_sibling() {
                    break;
                }
            }
        }

        if node.is_error() || last_error {
            diagnostics.push(Diagnostic {
                range: Range {
                    start: Position {
                        line: node.range().start_point.row as u32,
                        character: node.range().start_point.column as u32,
                    },
                    end: Position {
                        line: node.range().end_point.row as u32,
                        character: node.range().end_point.column as u32,
                    },
                },
                severity: Some(DiagnosticSeverity::Error),
                code: None,
                code_description: None,
                source: None,
                message: "Syntax error".to_string(),
                related_information: None,
                tags: None,
                data: None,
            });
        }
    }

    async fn report_syntax_errors(&self) {
        let tree = self.document.read().await.clone().unwrap();
        let mut diagnostics = Vec::new();
        {
            let node = tree.root_node();
            if !node.has_error() {
                return;
            }
            self.collect_syntax_errors(tree.root_node(), &mut diagnostics);
        }
        println!("{:?}", diagnostics);
        let uri = self.url.read().await.clone();
        self._client
            .publish_diagnostics(uri.unwrap(), diagnostics, None)
            .await;
    }
}

#[tower_lsp::async_trait]
impl LanguageServer for Backend {
    async fn initialize(&self, _params: InitializeParams) -> TResult<InitializeResult> {
        let mut result = InitializeResult::default();

        // let workspace_server_capabilities = WorkspaceServerCapabilities {
        //     workspace_folders: Some(WorkspaceFoldersServerCapabilities {
        //         supported: Some(true),
        //         change_notifications: Some(OneOf::Left(false)),
        //     }),
        //     file_operations: WorkspaceFileOperationsServerCapabilities {
        //         did_create: (),
        //         will_create: (),
        //         did_rename: (),
        //         will_rename: (),
        //         did_delete: (),
        //         will_delete: (),
        //     }
        // };

        result.capabilities.hover_provider = Some(HoverProviderCapability::Simple(true));
        result.capabilities.text_document_sync =
            Some(TextDocumentSyncCapability::Kind(TextDocumentSyncKind::Full));
        result.server_info = Some(ServerInfo {
            name: "teal-lsp".to_string(),
            version: Some(env!("CARGO_PKG_VERSION").to_string()),
        });
        Ok(result)
    }

    async fn shutdown(&self) -> TResult<()> {
        Ok(())
    }

    async fn initialized(&self, _params: InitializedParams) {}

    async fn did_change_workspace_folders(&self, params: DidChangeWorkspaceFoldersParams) {
        let _ = params;
        warn!("Got a workspace/didChangeWorkspaceFolders notification, but it is not implemented");
    }

    async fn did_change_configuration(&self, params: DidChangeConfigurationParams) {
        let _ = params;
        warn!("Got a workspace/didChangeConfiguration notification, but it is not implemented");
    }

    async fn did_change_watched_files(&self, params: DidChangeWatchedFilesParams) {
        let _ = params;
        warn!("Got a workspace/didChangeWatchedFiles notification, but it is not implemented");
    }

    async fn symbol(
        &self,
        params: WorkspaceSymbolParams,
    ) -> TResult<Option<Vec<SymbolInformation>>> {
        let _ = params;
        error!("Got a workspace/symbol request, but it is not implemented");
        Err(tower_lsp::jsonrpc::Error::method_not_found())
    }

    async fn execute_command(&self, params: ExecuteCommandParams) -> TResult<Option<Value>> {
        let _ = params;
        error!("Got a workspace/executeCommand request, but it is not implemented");
        Err(tower_lsp::jsonrpc::Error::method_not_found())
    }

    async fn will_create_files(&self, params: CreateFilesParams) -> TResult<Option<WorkspaceEdit>> {
        let _ = params;
        error!("Got a workspace/willCreateFiles request, but it is not implemented");
        Err(tower_lsp::jsonrpc::Error::method_not_found())
    }

    async fn did_create_files(&self, params: CreateFilesParams) {
        let _ = params;
        warn!("Got a workspace/didCreateFiles notification, but it is not implemented");
    }

    async fn will_rename_files(&self, params: RenameFilesParams) -> TResult<Option<WorkspaceEdit>> {
        let _ = params;
        error!("Got a workspace/willRenameFiles request, but it is not implemented");
        Err(tower_lsp::jsonrpc::Error::method_not_found())
    }

    async fn did_rename_files(&self, params: RenameFilesParams) {
        let _ = params;
        warn!("Got a workspace/didRenameFiles notification, but it is not implemented");
    }

    async fn will_delete_files(&self, params: DeleteFilesParams) -> TResult<Option<WorkspaceEdit>> {
        let _ = params;
        error!("Got a workspace/willDeleteFiles request, but it is not implemented");
        Err(tower_lsp::jsonrpc::Error::method_not_found())
    }

    async fn did_delete_files(&self, params: DeleteFilesParams) {
        let _ = params;
        warn!("Got a workspace/didDeleteFiles notification, but it is not implemented");
    }

    async fn did_open(&self, params: DidOpenTextDocumentParams) {
        {
            let mut url = self.url.write().await;
            *url = Some(params.text_document.uri.clone());
        }
        let mut source = self.source.write().await;
        *source = params.text_document.text;
        let source = {
            let s = source.clone();
            drop(source);
            s
        };
        let mut parser = get_parser();
        let mut document = self.document.write().await;
        *document = parser.parse(source.clone(), None);
        drop(document);

        // let mut cursor = match *document {
        //     Some(ref doc) => doc.walk(),
        //     None => unreachable!(),
        // };
        // println!("{}", pretty_print(&source, cursor.node(), 0));
        self.report_syntax_errors().await;
    }

    async fn did_change(&self, params: DidChangeTextDocumentParams) {
        println!("Did change: {:?}", params);
        let _ = params;
        warn!("Got a textDocument/didChange notification, but it is not implemented");
    }

    async fn will_save(&self, params: WillSaveTextDocumentParams) {
        let _ = params;
        warn!("Got a textDocument/willSave notification, but it is not implemented");
    }

    async fn will_save_wait_until(
        &self,
        params: WillSaveTextDocumentParams,
    ) -> TResult<Option<Vec<TextEdit>>> {
        let _ = params;
        error!("Got a textDocument/willSaveWaitUntil request, but it is not implemented");
        Err(tower_lsp::jsonrpc::Error::method_not_found())
    }

    async fn did_save(&self, params: DidSaveTextDocumentParams) {
        let _ = params;
        warn!("Got a textDocument/didSave notification, but it is not implemented");
    }

    async fn did_close(&self, params: DidCloseTextDocumentParams) {
        let _ = params;
        warn!("Got a textDocument/didClose notification, but it is not implemented");
    }

    async fn completion(&self, params: CompletionParams) -> TResult<Option<CompletionResponse>> {
        let _ = params;
        error!("Got a textDocument/completion request, but it is not implemented");
        Err(tower_lsp::jsonrpc::Error::method_not_found())
    }

    async fn completion_resolve(&self, params: CompletionItem) -> TResult<CompletionItem> {
        let _ = params;
        error!("Got a completionItem/resolve request, but it is not implemented");
        Err(tower_lsp::jsonrpc::Error::method_not_found())
    }

    async fn hover(&self, _params: HoverParams) -> TResult<Option<Hover>> {
        println!("Hover");
        Ok(Some(Hover {
            contents: HoverContents::Scalar(MarkedString::String("Hover".to_string())),
            range: None,
        }))
    }

    async fn signature_help(&self, params: SignatureHelpParams) -> TResult<Option<SignatureHelp>> {
        let _ = params;
        error!("Got a textDocument/signatureHelp request, but it is not implemented");
        Err(tower_lsp::jsonrpc::Error::method_not_found())
    }

    async fn goto_declaration(
        &self,
        params: request::GotoDeclarationParams,
    ) -> TResult<Option<request::GotoDeclarationResponse>> {
        let _ = params;
        error!("Got a textDocument/declaration request, but it is not implemented");
        Err(tower_lsp::jsonrpc::Error::method_not_found())
    }

    async fn goto_definition(
        &self,
        params: GotoDefinitionParams,
    ) -> TResult<Option<GotoDefinitionResponse>> {
        let _ = params;
        error!("Got a textDocument/definition request, but it is not implemented");
        Err(tower_lsp::jsonrpc::Error::method_not_found())
    }

    async fn goto_type_definition(
        &self,
        params: request::GotoTypeDefinitionParams,
    ) -> TResult<Option<request::GotoTypeDefinitionResponse>> {
        let _ = params;
        error!("Got a textDocument/typeDefinition request, but it is not implemented");
        Err(tower_lsp::jsonrpc::Error::method_not_found())
    }

    async fn goto_implementation(
        &self,
        params: request::GotoImplementationParams,
    ) -> TResult<Option<request::GotoImplementationResponse>> {
        let _ = params;
        error!("Got a textDocument/implementation request, but it is not implemented");
        Err(tower_lsp::jsonrpc::Error::method_not_found())
    }

    async fn references(&self, params: ReferenceParams) -> TResult<Option<Vec<Location>>> {
        let _ = params;
        error!("Got a textDocument/references request, but it is not implemented");
        Err(tower_lsp::jsonrpc::Error::method_not_found())
    }

    async fn document_highlight(
        &self,
        params: DocumentHighlightParams,
    ) -> TResult<Option<Vec<DocumentHighlight>>> {
        let _ = params;
        error!("Got a textDocument/documentHighlight request, but it is not implemented");
        Err(tower_lsp::jsonrpc::Error::method_not_found())
    }

    async fn document_symbol(
        &self,
        params: DocumentSymbolParams,
    ) -> TResult<Option<DocumentSymbolResponse>> {
        let _ = params;
        error!("Got a textDocument/documentSymbol request, but it is not implemented");
        Err(tower_lsp::jsonrpc::Error::method_not_found())
    }

    async fn code_action(&self, params: CodeActionParams) -> TResult<Option<CodeActionResponse>> {
        let _ = params;
        error!("Got a textDocument/codeAction request, but it is not implemented");
        Err(tower_lsp::jsonrpc::Error::method_not_found())
    }

    async fn code_action_resolve(&self, params: CodeAction) -> TResult<CodeAction> {
        let _ = params;
        error!("Got a codeAction/resolve request, but it is not implemented");
        Err(tower_lsp::jsonrpc::Error::method_not_found())
    }

    async fn code_lens(&self, params: CodeLensParams) -> TResult<Option<Vec<CodeLens>>> {
        let _ = params;
        error!("Got a textDocument/codeLens request, but it is not implemented");
        Err(tower_lsp::jsonrpc::Error::method_not_found())
    }

    async fn code_lens_resolve(&self, params: CodeLens) -> TResult<CodeLens> {
        let _ = params;
        error!("Got a codeLens/resolve request, but it is not implemented");
        Err(tower_lsp::jsonrpc::Error::method_not_found())
    }

    async fn document_link(
        &self,
        params: DocumentLinkParams,
    ) -> TResult<Option<Vec<DocumentLink>>> {
        let _ = params;
        error!("Got a textDocument/documentLink request, but it is not implemented");
        Err(tower_lsp::jsonrpc::Error::method_not_found())
    }

    async fn document_link_resolve(&self, params: DocumentLink) -> TResult<DocumentLink> {
        let _ = params;
        error!("Got a documentLink/resolve request, but it is not implemented");
        Err(tower_lsp::jsonrpc::Error::method_not_found())
    }

    async fn document_color(&self, params: DocumentColorParams) -> TResult<Vec<ColorInformation>> {
        let _ = params;
        error!("Got a textDocument/documentColor request, but it is not implemented");
        Err(tower_lsp::jsonrpc::Error::method_not_found())
    }

    async fn color_presentation(
        &self,
        params: ColorPresentationParams,
    ) -> TResult<Vec<ColorPresentation>> {
        let _ = params;
        error!("Got a textDocument/colorPresentation request, but it is not implemented");
        Err(tower_lsp::jsonrpc::Error::method_not_found())
    }

    async fn formatting(&self, params: DocumentFormattingParams) -> TResult<Option<Vec<TextEdit>>> {
        let _ = params;
        error!("Got a textDocument/formatting request, but it is not implemented");
        Err(tower_lsp::jsonrpc::Error::method_not_found())
    }

    async fn range_formatting(
        &self,
        params: DocumentRangeFormattingParams,
    ) -> TResult<Option<Vec<TextEdit>>> {
        let _ = params;
        error!("Got a textDocument/rangeFormatting request, but it is not implemented");
        Err(tower_lsp::jsonrpc::Error::method_not_found())
    }

    async fn on_type_formatting(
        &self,
        params: DocumentOnTypeFormattingParams,
    ) -> TResult<Option<Vec<TextEdit>>> {
        let _ = params;
        error!("Got a textDocument/onTypeFormatting request, but it is not implemented");
        Err(tower_lsp::jsonrpc::Error::method_not_found())
    }

    async fn rename(&self, params: RenameParams) -> TResult<Option<WorkspaceEdit>> {
        let _ = params;
        error!("Got a textDocument/rename request, but it is not implemented");
        Err(tower_lsp::jsonrpc::Error::method_not_found())
    }

    async fn prepare_rename(
        &self,
        params: TextDocumentPositionParams,
    ) -> TResult<Option<PrepareRenameResponse>> {
        let _ = params;
        error!("Got a textDocument/prepareRename request, but it is not implemented");
        Err(tower_lsp::jsonrpc::Error::method_not_found())
    }

    async fn folding_range(
        &self,
        params: FoldingRangeParams,
    ) -> TResult<Option<Vec<FoldingRange>>> {
        let _ = params;
        error!("Got a textDocument/foldingRange request, but it is not implemented");
        Err(tower_lsp::jsonrpc::Error::method_not_found())
    }

    async fn selection_range(
        &self,
        params: SelectionRangeParams,
    ) -> TResult<Option<Vec<SelectionRange>>> {
        let _ = params;
        error!("Got a textDocument/selectionRange request, but it is not implemented");
        Err(tower_lsp::jsonrpc::Error::method_not_found())
    }

    async fn prepare_call_hierarchy(
        &self,
        params: CallHierarchyPrepareParams,
    ) -> TResult<Option<Vec<CallHierarchyItem>>> {
        let _ = params;
        error!("Got a textDocument/prepareCallHierarchy request, but it is not implemented");
        Err(tower_lsp::jsonrpc::Error::method_not_found())
    }

    async fn incoming_calls(
        &self,
        params: CallHierarchyIncomingCallsParams,
    ) -> TResult<Option<Vec<CallHierarchyIncomingCall>>> {
        let _ = params;
        error!("Got a callHierarchy/incomingCalls request, but it is not implemented");
        Err(tower_lsp::jsonrpc::Error::method_not_found())
    }

    async fn outgoing_calls(
        &self,
        params: CallHierarchyOutgoingCallsParams,
    ) -> TResult<Option<Vec<CallHierarchyOutgoingCall>>> {
        let _ = params;
        error!("Got a callHierarchy/outgoingCalls request, but it is not implemented");
        Err(tower_lsp::jsonrpc::Error::method_not_found())
    }

    async fn semantic_tokens_full(
        &self,
        params: SemanticTokensParams,
    ) -> TResult<Option<SemanticTokensResult>> {
        let _ = params;
        error!("Got a textDocument/semanticTokens/full request, but it is not implemented");
        Err(tower_lsp::jsonrpc::Error::method_not_found())
    }

    async fn semantic_tokens_full_delta(
        &self,
        params: SemanticTokensDeltaParams,
    ) -> TResult<Option<SemanticTokensFullDeltaResult>> {
        let _ = params;
        error!("Got a textDocument/semanticTokens/full/delta request, but it is not implemented");
        Err(tower_lsp::jsonrpc::Error::method_not_found())
    }

    async fn semantic_tokens_range(
        &self,
        params: SemanticTokensRangeParams,
    ) -> TResult<Option<SemanticTokensRangeResult>> {
        let _ = params;
        error!("Got a textDocument/semanticTokens/range request, but it is not implemented");
        Err(tower_lsp::jsonrpc::Error::method_not_found())
    }

    async fn linked_editing_range(
        &self,
        params: LinkedEditingRangeParams,
    ) -> TResult<Option<LinkedEditingRanges>> {
        let _ = params;
        error!("Got a textDocument/linkedEditingRange request, but it is not implemented");
        Err(tower_lsp::jsonrpc::Error::method_not_found())
    }

    async fn moniker(&self, params: MonikerParams) -> TResult<Option<Vec<Moniker>>> {
        let _ = params;
        error!("Got a textDocument/moniker request, but it is not implemented");
        Err(tower_lsp::jsonrpc::Error::method_not_found())
    }
}

#[tokio::main]
async fn main() -> Result<(), std::io::Error> {
    // let stdin = tokio::io::stdin();
    // let stdout = tokio::io::stdout();
    loop {
        let listener = tokio::net::TcpListener::bind("127.0.0.1:9527").await?;
        let (stream, _) = listener.accept().await?;
        let (read, write) = tokio::io::split(stream);

        let (service, messages) = LspService::new(|client| Backend {
            _client: client,
            document: Arc::new(RwLock::new(None)),
            source: Arc::new(RwLock::new(String::new())),
            url: Arc::new(RwLock::new(None)),
        });

        Server::new(read, write)
            .interleave(messages)
            .serve(service)
            .await;
    }
}
