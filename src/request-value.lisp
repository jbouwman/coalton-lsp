;;;; LSP message parameter value definitions
;;;;
;;;; A strict translation of the interfaces defined by the Language
;;;; Server Protocol 3.17 spec: lines that are prefixed with ;;;
;;;; | are derived directly from it.
;;;;
;;;; The DEFINE-* macros used here are defined in lib/message.lisp.
;;;;
;;;; https://microsoft.github.io/language-server-protocol/specifications/lsp/3.17/specification/

(in-package #:coalton-lsp)

(define-atom string)

(define-atom boolean)

(define-atom integer)

(deftype uinteger ()
  '(integer 0 #.(1- (expt 2 31))))

(define-atom uinteger)

(define-atom decimal)

(define-atom lsp-any)

(define-class message ()
  (:jsonrpc string))

(define-class request-message (message)
  (:id integer)
  (:method string)
  (:params (lsp-any :optional t)))

(define-enum error-code ()
  (:unknown-error-code -32001)
  (:server-not-initialized -32002)
  (:invalid-request -32600)
  (:method-not-found -32601)
  (:invalid-params -32602)
  (:internal-error -32603)
  (:parse-error -32700)
  (:request-cancelled -32800)
  (:content-modified -32801)
  (:server-cancelled -32802)
  (:request-failed -32803))

(define-class response-error ()
  (:code error-code)
  (:message string)
  (:data (lsp-any :optional t)))

(define-union response-message-id (integer string))

(define-class response-message (message)
  (:id (response-message-id :optional t))
  (:result (lsp-any :optional t))
  (:error (response-error :optional t)))

(define-class notification-message (message)
  (:method string)
  (:params (lsp-any :optional t)))

(define-union progress-token (integer string))

(define-class cancel-params ()
  (:id progress-token))

(define-class progress-params ()
  (:id progress-token)
  (:value lsp-any))

;;; | interface HoverParams {
;;; |     textDocument: string; position: { line: uinteger; character: uinteger; };
;;; | }

;;; | interface HoverResult {
;;; |     value: string;
;;; | }

(deftype document-uri () 'string)

(define-atom document-uri)

(deftype uri () 'string)

(define-atom uri)

;;; | interface RegularExpressionsClientCapabilities {
;;; |     engine: string;
;;; |     version?: string;
;;; | }
;;; | EOL: string[] = ['\n', '\r\n', '\r'];

(define-class position ()
  (:line uinteger)
  (:character uinteger))

(define-enum position-encoding-kind ()
  (:utf8 "utf-8")
  (:utf16 "utf-16")
  (:utf32 "utf-32"))

(define-class range ()
  (:start position)
  (:end position))

(define-class text-document-item ()
  (:uri uri)
  (:language-id string)
  (:version integer)
  (:text string))

(define-class text-document-identifier ()
  (:uri uri))

(define-class versioned-text-document-identifier (text-document-identifier)
  (:version integer))

(define-class optional-versioned-text-document-identifier (text-document-identifier)
  (:version (integer :optional t)))

;;; | interface TextDocumentPositionParams {
;;; |     textDocument: TextDocumentIdentifier;
;;; |     position: Position;
;;; | }

(define-class document-filter ()
  (:language (string :optional t))
  (:scheme (string :optional t))
  (:pattern (string :optional t)))

;;; | type DocumentSelector = DocumentFilter[];

;;; | interface TextEdit {
;;; |     range: Range;
;;; |     newText: string;
;;; | }

;;; | interface ChangeAnnotation {
;;; |     label: string;
;;; |     needsConfirmation?: boolean;
;;; |     description?: string;
;;; | }

;;; | type ChangeAnnotationIdentifier = string;

;;; | interface AnnotatedTextEdit extends TextEdit {
;;; |     annotationId: ChangeAnnotationIdentifier;
;;; | }

;;; | interface TextDocumentEdit {
;;; |     textDocument: OptionalVersionedTextDocumentIdentifier;
;;; |     edits: (TextEdit | AnnotatedTextEdit)[];
;;; | }

(define-class location ()
  (:uri uri)
  (:range range))

;;; | interface LocationLink {
;;; |     originSelectionRange?: Range;
;;; |     targetUri: DocumentUri;
;;; |     targetRange: Range;
;;; |     targetSelectionRange: Range;
;;; | }

(define-enum diagnostic-severity ()
  (:error 1)
  (:warning 2)
  (:information 3)
  (:hint 4))

(define-enum diagnostic-tag ()
  (:unnecessary 1)
  (:deprecated 2))

(define-class diagnostic-related-information ()
  (:location location)
  (:message string))

(define-union diagnostic-code (integer string))

(define-class diagnostic ()
  (:range range)
  (:severity (diagnostic-severity :optional t))
  (:code (diagnostic-code :optional t))
  (:source (string :optional t))
  (:message string)
  (:tags (diagnostic-tag :vector t :optional t))
  (:related-information (diagnostic-related-information :vector t :optional t))
  (:data (lsp-any :optional t)))

;;; | interface CodeDescription {
;;; |     href: URI;
;;; | }

;;; | interface Command {
;;; |     title: string;
;;; |     command: string;
;;; |     arguments?: LSPAny[];
;;; | }

(define-enum markup-kind ()
  (:plaintext "plaintext")
  (:markdown "markdown"))

;;; | interface MarkupContent {
;;; |     kind: MarkupKind;
;;; |     value: string;
;;; | }

;;; | interface MarkdownClientCapabilities {
;;; |     parser: string;
;;; |     version?: string;
;;; |     allowedTags?: string[];
;;; | }

;;; | interface CreateFileOptions {
;;; |     overwrite?: boolean;
;;; |     ignoreIfExists?: boolean;
;;; | }

;;; | interface CreateFile {
;;; |     kind: 'create';
;;; |     uri: DocumentUri;
;;; |     options?: CreateFileOptions;
;;; |     annotationId?: ChangeAnnotationIdentifier;
;;; | }

;;; | interface RenameFileOptions {
;;; |     overwrite?: boolean;
;;; |     ignoreIfExists?: boolean;
;;; | }

;;; | interface RenameFile {
;;; |     kind: 'rename';
;;; |     oldUri: DocumentUri;
;;; |     newUri: DocumentUri;
;;; |     options?: RenameFileOptions;
;;; |     annotationId?: ChangeAnnotationIdentifier;
;;; | }

;;; | interface DeleteFileOptions {
;;; |     recursive?: boolean;
;;; |     ignoreIfNotExists?: boolean;
;;; | }

;;; | interface DeleteFile {
;;; |     kind: 'delete';
;;; |     uri: DocumentUri;
;;; |     options?: DeleteFileOptions;
;;; |     annotationId?: ChangeAnnotationIdentifier;
;;; | }

;;; | interface WorkspaceEdit {
;;; |     changes?: { [uri: DocumentUri]: TextEdit[]; };
;;; |     documentChanges?: (
;;; |         TextDocumentEdit[] |
;;; |         (TextDocumentEdit | CreateFile | RenameFile | DeleteFile)[]
;;; |     );
;;; |     changeAnnotations?: {
;;; |         [id: string /* ChangeAnnotationIdentifier */]: ChangeAnnotation;
;;; |     };
;;; | }

(define-enum resource-operation-kind ()
  (:create "create")
  (:rename "rename")
  (:delete "delete"))

(define-enum failure-handling-kind ()
  (:abort "abort")
  (:transactional "transactional")
  (:undo "undo")
  (:text-only-transactional "textOnlyTransactional"))

;;; | interface Registration {
;;; |     id: string;
;;; |     method: string;
;;; |     registerOptions?: LSPAny;
;;; | }

;;; | interface RegistrationParams {
;;; |     registrations: Registration[];
;;; | }

(define-class static-registration-options ()
  (:id (string :optional t)))

(define-class text-document-registration-options ()
  (:document-selector (document-filter :vector t :optional t)))

;;; | interface Unregistration {
;;; |     id: string;
;;; |     method: string;
;;; | }

;;; | interface UnregistrationParams {
;;; |     unregisterations: Unregistration[];
;;; | }

;;; | interface SetTraceParams {
;;; |     value: TraceValue;
;;; | }

;;; | interface LogTraceParams {
;;; |     message: string;
;;; |     verbose?: string;
;;; | }

(define-enum text-document-sync-kind ()
  (:none 0)
  (:full 1)
  (:incremental 2))

(define-class text-document-sync-options ()
  (:open-close (boolean :optional t))
  (:change (text-document-sync-kind :optional t)))

(define-class did-open-text-document-params ()
  (:text-document text-document-item))

;;; | interface TextDocumentChangeRegistrationOptions
;;; |     extends TextDocumentRegistrationOptions {
;;; |     syncKind: TextDocumentSyncKind;
;;; | }

(define-class did-change-text-document-params ()
  (:text-document versioned-text-document-identifier))

;;; | type TextDocumentContentChangeEvent = {
;;; |     range: Range;
;;; |     rangeLength?: uinteger;
;;; |     text: string;
;;; | } | {
;;; |     text: string;
;;; | };

;;; | interface WillSaveTextDocumentParams {
;;; |     textDocument: TextDocumentIdentifier;
;;; |     reason: TextDocumentSaveReason;
;;; | }

;;; | namespace TextDocumentSaveReason {
;;; |     Manual = 1;
;;; |     AfterDelay = 2;
;;; |     FocusOut = 3;
;;; | }

;;; | type TextDocumentSaveReason = 1 | 2 | 3;

;;; | interface SaveOptions {
;;; |     includeText?: boolean;
;;; | }

;;; | interface TextDocumentSaveRegistrationOptions
;;; |     extends TextDocumentRegistrationOptions {
;;; |     includeText?: boolean;
;;; | }

;;; | interface DidSaveTextDocumentParams {
;;; |     textDocument: TextDocumentIdentifier;
;;; |     text?: string;
;;; | }

;;; | interface DidCloseTextDocumentParams {
;;; |     textDocument: TextDocumentIdentifier;
;;; | }

;;; | interface TextDocumentSyncOptions {
;;; |     openClose?: boolean;
;;; |     change?: TextDocumentSyncKind;
;;; |     willSave?: boolean;
;;; |     willSaveWaitUntil?: boolean;
;;; |     save?: boolean | SaveOptions;
;;; | }

;;; | interface NotebookDocument {
;;; |     uri: URI;
;;; |     notebookType: string;
;;; |     version: integer;
;;; |     metadata?: LSPObject;
;;; |     cells: NotebookCell[];
;;; | }

;;; | interface NotebookCell {
;;; |     kind: NotebookCellKind;
;;; |     document: DocumentUri;
;;; |     metadata?: LSPObject;
;;; |     executionSummary?: ExecutionSummary;
;;; | }

;;; | namespace NotebookCellKind {
;;; |     Markup: 1 = 1;
;;; |     Code: 2 = 2;
;;; | }

;;; | interface ExecutionSummary {
;;; |     executionOrder: uinteger;
;;; |     success?: boolean;
;;; | }

;;; | interface NotebookCellTextDocumentFilter {
;;; |     notebook: string | NotebookDocumentFilter;
;;; |     language?: string;
;;; | }

;;; | type NotebookDocumentFilter = {
;;; |     notebookType: string;
;;; |     scheme?: string;
;;; |     pattern?: string;
;;; | } | {
;;; |     notebookType?: string;
;;; |     scheme: string;
;;; |     pattern?: string;
;;; | } | {
;;; |     notebookType?: string;
;;; |     scheme?: string;
;;; |     pattern: string;
;;; | };

;;; | interface NotebookDocumentSyncClientCapabilities {
;;; |     dynamicRegistration?: boolean;
;;; |     executionSummarySupport?: boolean;
;;; | }

;;; | interface NotebookDocumentSyncOptions {
;;; |     notebookSelector: ({
;;; |         notebook: string | NotebookDocumentFilter;
;;; |         cells?: { language: string }[];
;;; |     } | {
;;; |         notebook?: string | NotebookDocumentFilter;
;;; |         cells: { language: string }[];
;;; |     })[];
;;; |     save?: boolean;
;;; | }

;;; | interface NotebookDocumentSyncRegistrationOptions extends
;;; |     NotebookDocumentSyncOptions, StaticRegistrationOptions {
;;; | }

;;; | interface DidOpenNotebookDocumentParams {
;;; |     notebookDocument: NotebookDocument;
;;; |     cellTextDocuments: TextDocumentItem[];
;;; | }

;;; | interface DidChangeNotebookDocumentParams {
;;; |     notebookDocument: VersionedNotebookDocumentIdentifier;
;;; |     change: NotebookDocumentChangeEvent;
;;; | }

;;; | interface VersionedNotebookDocumentIdentifier {
;;; |     version: integer;
;;; |     uri: URI;
;;; | }

;;; | interface NotebookDocumentChangeEvent {
;;; |     metadata?: LSPObject;
;;; |     cells?: {
;;; |         structure?: {
;;; |             array: NotebookCellArrayChange;
;;; |             didOpen?: TextDocumentItem[];
;;; |             didClose?: TextDocumentIdentifier[];
;;; |         };
;;; |         data?: NotebookCell[];
;;; |         textContent?: {
;;; |             document: VersionedTextDocumentIdentifier;
;;; |             changes: TextDocumentContentChangeEvent[];
;;; |         }[];
;;; |     };
;;; | }

;;; | interface NotebookCellArrayChange {
;;; |     start: uinteger;
;;; |     deleteCount: uinteger;
;;; |     cells?: NotebookCell[];
;;; | }

;;; | interface DidSaveNotebookDocumentParams {
;;; |     notebookDocument: NotebookDocumentIdentifier;
;;; | }

;;; | interface DidCloseNotebookDocumentParams {
;;; |     notebookDocument: NotebookDocumentIdentifier;
;;; |     cellTextDocuments: TextDocumentIdentifier[];
;;; | }

;;; | interface NotebookDocumentIdentifier {
;;; |     uri: URI;
;;; | }

(define-class declaration-client-capabilities ()
  (:dynamic-registration (boolean :optional t))
  (:link-support (boolean :optional t)))

;;; | namespace FailureHandlingKind {
;;; |     Abort: FailureHandlingKind = 'abort';
;;; |     Transactional: FailureHandlingKind = 'transactional';
;;; |     TextOnlyTransactional: FailureHandlingKind
;;; |         = 'textOnlyTransactional';
;;; |     Undo: FailureHandlingKind = 'undo';
;;; | }

;;; | interface WorkDoneProgressBegin {
;;; |     kind: 'begin';
;;; |     title: string;
;;; |     cancellable?: boolean;
;;; |     message?: string;
;;; |     percentage?: uinteger;
;;; | }

;;; | interface WorkDoneProgressReport {
;;; |     kind: 'report';
;;; |     cancellable?: boolean;
;;; |     message?: string;
;;; |     percentage?: uinteger;
;;; | }

;;; | interface WorkDoneProgressEnd {
;;; |     kind: 'end';
;;; |     message?: string;
;;; | }

(define-class work-done-progress-params ()
  (:work-done-token (progress-token :optional t)))

(define-class work-done-progress-options ()
  (:work-done-progress (boolean :optional t)))

(define-class partial-result-params ()
  (:partial-result-token (progress-token :optional t)))

(define-enum trace-value ()
  (:off "off")
  (:messages "messages")
  (:verbose "verbose"))

(define-class declaration-options (work-done-progress-options))

;;; | interface DeclarationRegistrationOptions extends DeclarationOptions,
;;; |     TextDocumentRegistrationOptions, StaticRegistrationOptions {
;;; | }

;;; | interface DeclarationParams extends TextDocumentPositionParams,
;;; |     WorkDoneProgressParams, PartialResultParams {
;;; | }

(define-class definition-client-capabilities ()
  (:dynamic-registration (boolean :optional t))
  (:link-support (boolean :optional t)))

(define-class definition-options (work-done-progress-options))

;;; | interface DefinitionRegistrationOptions extends
;;; |     TextDocumentRegistrationOptions, DefinitionOptions {
;;; | }

;;; | interface DefinitionParams extends TextDocumentPositionParams,
;;; |     WorkDoneProgressParams, PartialResultParams {
;;; | }

(define-class type-definition-client-capabilities ()
  (:dynamic-registration (boolean :optional t))
  (:link-support (boolean :optional t)))

;;; | interface TypeDefinitionOptions extends WorkDoneProgressOptions {
;;; | }

;;; | interface TypeDefinitionRegistrationOptions extends
;;; |     TextDocumentRegistrationOptions, TypeDefinitionOptions,
;;; |     StaticRegistrationOptions {
;;; | }

;;; | interface TypeDefinitionParams extends TextDocumentPositionParams,
;;; |     WorkDoneProgressParams, PartialResultParams {
;;; | }

(define-class implementation-client-capabilities ()
  (:dynamic-registration (boolean :optional t))
  (:link-support (boolean :optional t)))

;;; | interface ImplementationOptions extends WorkDoneProgressOptions {
;;; | }

;;; | interface ImplementationRegistrationOptions extends
;;; |     TextDocumentRegistrationOptions, ImplementationOptions,
;;; |     StaticRegistrationOptions {
;;; | }

;;; | interface ImplementationParams extends TextDocumentPositionParams,
;;; |     WorkDoneProgressParams, PartialResultParams {
;;; | }

(define-class reference-client-capabilities ()
  (:dynamic-registration (boolean :optional t)))

;;; | interface ReferenceOptions extends WorkDoneProgressOptions {
;;; | }

;;; | interface ReferenceRegistrationOptions extends
;;; |     TextDocumentRegistrationOptions, ReferenceOptions {
;;; | }

;;; | interface ReferenceParams extends TextDocumentPositionParams,
;;; |     WorkDoneProgressParams, PartialResultParams {
;;; |     context: ReferenceContext;
;;; | }

;;; | interface ReferenceContext {
;;; |     includeDeclaration: boolean;
;;; | }

;;; | interface CallHierarchyClientCapabilities {
;;; |     dynamicRegistration?: boolean;
;;; | }

;;; | interface CallHierarchyOptions extends WorkDoneProgressOptions {
;;; | }

;;; | interface CallHierarchyRegistrationOptions extends
;;; |     TextDocumentRegistrationOptions, CallHierarchyOptions,
;;; |     StaticRegistrationOptions {
;;; | }

;;; | interface CallHierarchyPrepareParams extends TextDocumentPositionParams,
;;; |     WorkDoneProgressParams {
;;; | }

;;; | interface CallHierarchyItem {
;;; |     name: string;
;;; |     kind: SymbolKind;
;;; |     tags?: SymbolTag[];
;;; |     detail?: string;
;;; |     uri: DocumentUri;
;;; |     range: Range;
;;; |     selectionRange: Range;
;;; |     data?: LSPAny;
;;; | }

;;; | interface CallHierarchyIncomingCallsParams extends
;;; |     WorkDoneProgressParams, PartialResultParams {
;;; |     item: CallHierarchyItem;
;;; | }

;;; | interface CallHierarchyIncomingCall {
;;; |     from: CallHierarchyItem;
;;; |     fromRanges: Range[];
;;; | }

;;; | interface CallHierarchyOutgoingCallsParams extends
;;; |     WorkDoneProgressParams, PartialResultParams {
;;; |     item: CallHierarchyItem;
;;; | }

;;; | interface CallHierarchyOutgoingCall {
;;; |     to: CallHierarchyItem;
;;; |     fromRanges: Range[];
;;; | }

;;; | type TypeHierarchyClientCapabilities = {
;;; |     dynamicRegistration?: boolean;
;;; | };

;;; | interface TypeHierarchyOptions extends WorkDoneProgressOptions {
;;; | }

;;; | interface TypeHierarchyRegistrationOptions extends
;;; |     TextDocumentRegistrationOptions, TypeHierarchyOptions,
;;; |     StaticRegistrationOptions {
;;; | }

;;; | interface TypeHierarchyPrepareParams extends TextDocumentPositionParams,
;;; |     WorkDoneProgressParams {
;;; | }

;;; | interface TypeHierarchyItem {
;;; |     name: string;
;;; |     kind: SymbolKind;
;;; |     tags?: SymbolTag[];
;;; |     detail?: string;
;;; |     uri: DocumentUri;
;;; |     range: Range;
;;; |     selectionRange: Range;
;;; |     data?: LSPAny;
;;; | }

;;; | interface TypeHierarchySupertypesParams extends
;;; |     WorkDoneProgressParams, PartialResultParams {
;;; |     item: TypeHierarchyItem;
;;; | }

;;; | interface TypeHierarchySubtypesParams extends
;;; |     WorkDoneProgressParams, PartialResultParams {
;;; |     item: TypeHierarchyItem;
;;; | }

(define-class document-highlight-client-capabilities ()
  (:dynamic-registration (boolean :optional t)))

;;; | interface DocumentHighlightOptions extends WorkDoneProgressOptions {
;;; | }

;;; | interface DocumentHighlightRegistrationOptions extends
;;; |     TextDocumentRegistrationOptions, DocumentHighlightOptions {
;;; | }

;;; | interface DocumentHighlightParams extends TextDocumentPositionParams,
;;; |     WorkDoneProgressParams, PartialResultParams {
;;; | }

;;; | interface DocumentHighlight {
;;; |     range: Range;
;;; |     kind?: DocumentHighlightKind;
;;; | }

;;; | namespace DocumentHighlightKind {
;;; |     Text = 1;
;;; |     Read = 2;
;;; |     Write = 3;
;;; | }

;;; | type DocumentHighlightKind = 1 | 2 | 3;

;;; | interface DocumentLinkClientCapabilities {
;;; |     dynamicRegistration?: boolean;
;;; |     tooltipSupport?: boolean;
;;; | }

(define-class document-link-options (work-done-progress-options)
  (:resolve-provider (boolean :optional t)))

;;; | interface DocumentLinkRegistrationOptions extends
;;; |     TextDocumentRegistrationOptions, DocumentLinkOptions {
;;; | }

;;; | interface DocumentLinkParams extends WorkDoneProgressParams,
;;; |     PartialResultParams {
;;; |     textDocument: TextDocumentIdentifier;
;;; | }

;;; | interface DocumentLink {
;;; |     range: Range;
;;; |     target?: URI;
;;; |     tooltip?: string;
;;; |     data?: LSPAny;
;;; | }

(define-class hover-client-capabilities ()
  (:dynamic-registration (boolean :optional t))
  (:content-format (markup-kind :vector t :optional t)))

;;; | interface HoverOptions extends WorkDoneProgressOptions {
;;; | }

;;; | interface HoverRegistrationOptions
;;; |     extends TextDocumentRegistrationOptions, HoverOptions {
;;; | }

;;; | interface HoverParams extends TextDocumentPositionParams,
;;; |     WorkDoneProgressParams {
;;; | }

;;; | interface Hover {
;;; |     contents: MarkedString | MarkedString[] | MarkupContent;
;;; |     range?: Range;
;;; | }

;;; | type MarkedString = string | { language: string; value: string };

;;; | interface CodeLensClientCapabilities {
;;; |     dynamicRegistration?: boolean;
;;; | }

;;; | interface CodeLensOptions extends WorkDoneProgressOptions {
;;; |     resolveProvider?: boolean;
;;; | }

;;; | interface CodeLensRegistrationOptions extends
;;; |     TextDocumentRegistrationOptions, CodeLensOptions {
;;; | }

;;; | interface CodeLensParams extends WorkDoneProgressParams, PartialResultParams {
;;; |     textDocument: TextDocumentIdentifier;
;;; | }

;;; | interface CodeLens {
;;; |     range: Range;
;;; |     command?: Command;
;;; |     data?: LSPAny;
;;; | }

(define-class code-lens-workspace-client-capabilities ()
  (:refresh-support (boolean :optional t)))

;;; | interface FoldingRangeClientCapabilities {
;;; |     dynamicRegistration?: boolean;
;;; |     rangeLimit?: uinteger;
;;; |     lineFoldingOnly?: boolean;
;;; |     foldingRangeKind?: {
;;; |         valueSet?: FoldingRangeKind[];
;;; |     };
;;; |     foldingRange?: {
;;; |         collapsedText?: boolean;
;;; |     };
;;; | }

;;; | interface FoldingRangeOptions extends WorkDoneProgressOptions {
;;; | }

;;; | interface FoldingRangeRegistrationOptions extends
;;; |     TextDocumentRegistrationOptions, FoldingRangeOptions,
;;; |     StaticRegistrationOptions {
;;; | }

;;; | interface FoldingRangeParams extends WorkDoneProgressParams,
;;; |     PartialResultParams {
;;; |     textDocument: TextDocumentIdentifier;
;;; | }

;;; | namespace FoldingRangeKind {
;;; |     Comment = 'comment';
;;; |     Imports = 'imports';
;;; |     Region = 'region';
;;; | }

;;; | type FoldingRangeKind = string;

;;; | interface FoldingRange {
;;; |     startLine: uinteger;
;;; |     startCharacter?: uinteger;
;;; |     endLine: uinteger;
;;; |     endCharacter?: uinteger;
;;; |     kind?: FoldingRangeKind;
;;; |     collapsedText?: string;
;;; | }

;;; | interface SelectionRangeClientCapabilities {
;;; |     dynamicRegistration?: boolean;
;;; | }

;;; | interface SelectionRangeOptions extends WorkDoneProgressOptions {
;;; | }

;;; | interface SelectionRangeRegistrationOptions extends
;;; |     SelectionRangeOptions, TextDocumentRegistrationOptions,
;;; |     StaticRegistrationOptions {
;;; | }

;;; | interface SelectionRangeParams extends WorkDoneProgressParams,
;;; |     PartialResultParams {
;;; |     textDocument: TextDocumentIdentifier;
;;; |     positions: Position[];
;;; | }

;;; | interface SelectionRange {
;;; |     range: Range;
;;; |     parent?: SelectionRange;
;;; | }

(define-class document-symbol-options (work-done-progress-options)
  (:label (string :optional t)))

;;; | interface DocumentSymbolRegistrationOptions extends
;;; |     TextDocumentRegistrationOptions, DocumentSymbolOptions {
;;; | }

(define-class document-symbol-params (work-done-progress-params
                                      partial-result-params)
  (:text-document text-document-identifier))

(define-enum symbol-kind ()
  (:file 1)
  (:module 2)
  (:namespace 3)
  (:package 4)
  (:class 5)
  (:method 6)
  (:property 7)
  (:field 8)
  (:constructor 9)
  (:enum 10)
  (:interface 11)
  (:function 12)
  (:variable 13)
  (:constant 14)
  (:string 15)
  (:number 16)
  (:boolean 17)
  (:array 18)
  (:object 19)
  (:key 20)
  (:null 21)
  (:enum-member 22)
  (:struct 23)
  (:event 24)
  (:operator 25)
  (:type-parameter 26))

(define-class symbol-kind-value-set ()
  (:value-set (symbol-kind :vector t)))

(define-enum symbol-tag ()
  (:deprecated 1))

(define-class symbol-tag-value-set ()
  (:value-set (symbol-tag :vector t)))

(define-class document-symbol-client-capabilities ()
  (:dynamic-registration (boolean :optional t))
  (:symbol-kind (symbol-kind-value-set :optional t))
  (:hierarchical-document-symbol-support (boolean :optional t))
  (:tag-support (symbol-tag-value-set :optional t))
  (:label-support (boolean :optional t)))

(define-class document-symbol ()
  (:name string)
  (:detail (string :optional T))
  (:kind symbol-kind)
  (:tags (symbol-tag :optional t :vector t))
  (:deprecated (boolean :optional t))
  (:range range)
  (:selection-range range)
  (:children (document-symbol :optional t :vector t)))

;;; | interface SymbolInformation {
;;; |     name: string;
;;; |     kind: SymbolKind;
;;; |     tags?: SymbolTag[];
;;; |     deprecated?: boolean;
;;; |     location: Location;
;;; |     containerName?: string;
;;; | }

;;; | enum SemanticTokenTypes {
;;; |     namespace = 'namespace',
;;; |     type = 'type',
;;; |     class = 'class',
;;; |     enum = 'enum',
;;; |     interface = 'interface',
;;; |     struct = 'struct',
;;; |     typeParameter = 'typeParameter',
;;; |     parameter = 'parameter',
;;; |     variable = 'variable',
;;; |     property = 'property',
;;; |     enumMember = 'enumMember',
;;; |     event = 'event',
;;; |     function = 'function',
;;; |     method = 'method',
;;; |     macro = 'macro',
;;; |     keyword = 'keyword',
;;; |     modifier = 'modifier',
;;; |     comment = 'comment',
;;; |     string = 'string',
;;; |     number = 'number',
;;; |     regexp = 'regexp',
;;; |     operator = 'operator',
;;; |     decorator = 'decorator'
;;; | }

;;; | enum SemanticTokenModifiers {
;;; |     declaration = 'declaration',
;;; |     definition = 'definition',
;;; |     readonly = 'readonly',
;;; |     static = 'static',
;;; |     deprecated = 'deprecated',
;;; |     abstract = 'abstract',
;;; |     async = 'async',
;;; |     modification = 'modification',
;;; |     documentation = 'documentation',
;;; |     defaultLibrary = 'defaultLibrary'
;;; | }

;;; | namespace TokenFormat {
;;; |     Relative: 'relative' = 'relative';
;;; | }

;;; | type TokenFormat = 'relative';

(define-class semantic-tokens-legend ()
  (:token-types (string :vector t))
  (:token-modifiers (string :vector t)))

;;; | interface SemanticTokensClientCapabilities {
;;; |     dynamicRegistration?: boolean;
;;; |     requests: {
;;; |         range?: boolean | {
;;; |         };
;;; |         full?: boolean | {
;;; |             delta?: boolean;
;;; |         };
;;; |     };
;;; |     tokenTypes: string[];
;;; |     tokenModifiers: string[];
;;; |     formats: TokenFormat[];
;;; |     overlappingTokenSupport?: boolean;
;;; |     multilineTokenSupport?: boolean;
;;; |     serverCancelSupport?: boolean;
;;; |     augmentsSyntaxTokens?: boolean;
;;; | }

(define-class delta ()
  (:delta (boolean :optional t)))

(define-union full-option (boolean delta))

(define-class empty ())

(define-union range-option (boolean empty))

(define-class semantic-tokens-options (work-done-progress-options)
  (:legend semantic-tokens-legend)
  (:range (range-option :optional t))
  (:full (full-option :optional t)))

(define-class semantic-tokens-registration-options (text-document-registration-options
                                                      semantic-tokens-options
                                                      static-registration-options))

;;; | interface SemanticTokensParams extends WorkDoneProgressParams,
;;; |     PartialResultParams {
;;; |     textDocument: TextDocumentIdentifier;
;;; | }

(define-class semantic-tokens ()
  (:result-id (string :optional t))
  (:data (uinteger :vector t)))

;;; | interface SemanticTokensPartialResult {
;;; |     data: uinteger[];
;;; | }

;;; | interface SemanticTokensDeltaParams extends WorkDoneProgressParams,
;;; |     PartialResultParams {
;;; |     textDocument: TextDocumentIdentifier;
;;; |     previousResultId: string;
;;; | }

;;; | interface SemanticTokensDelta {
;;; |     readonly resultId?: string;
;;; |     edits: SemanticTokensEdit[];
;;; | }

;;; | interface SemanticTokensEdit {
;;; |     start: uinteger;
;;; |     deleteCount: uinteger;
;;; |     data?: uinteger[];
;;; | }

;;; | interface SemanticTokensDeltaPartialResult {
;;; |     edits: SemanticTokensEdit[];
;;; | }

(define-class semantic-tokens-range-params (work-done-progress-params
                                              partial-result-params)
  (:text-document text-document-identifier)
  (:range range))

(define-class semantic-tokens-workspace-client-capabilities ()
  (:refresh-support (boolean :optional t)))

;;; | interface InlayHintClientCapabilities {
;;; |     dynamicRegistration?: boolean;
;;; |     resolveSupport?: {
;;; |         properties: string[];
;;; |     };
;;; | }

;;; | interface InlayHintOptions extends WorkDoneProgressOptions {
;;; |     resolveProvider?: boolean;
;;; | }

;;; | interface InlayHintRegistrationOptions extends InlayHintOptions,
;;; |     TextDocumentRegistrationOptions, StaticRegistrationOptions {
;;; | }

;;; | interface InlayHintParams extends WorkDoneProgressParams {
;;; |     textDocument: TextDocumentIdentifier;
;;; |     range: Range;
;;; | }

;;; | interface InlayHint {
;;; |     position: Position;
;;; |     label: string | InlayHintLabelPart[];
;;; |     kind?: InlayHintKind;
;;; |     textEdits?: TextEdit[];
;;; |     tooltip?: string | MarkupContent;
;;; |     paddingLeft?: boolean;
;;; |     paddingRight?: boolean;
;;; |     data?: LSPAny;
;;; | }

;;; | interface InlayHintLabelPart {
;;; |     value: string;
;;; |     tooltip?: string | MarkupContent;
;;; |     location?: Location;
;;; |     command?: Command;
;;; | }

;;; | namespace InlayHintKind {
;;; |     Type = 1;
;;; |     Parameter = 2;
;;; | }

;;; | type InlayHintKind = 1 | 2;

;;; | interface InlayHintWorkspaceClientCapabilities {
;;; |     refreshSupport?: boolean;
;;; | }

;;; | interface InlineValueClientCapabilities {
;;; |     dynamicRegistration?: boolean;
;;; | }

;;; | interface InlineValueOptions extends WorkDoneProgressOptions {
;;; | }

;;; | interface InlineValueRegistrationOptions extends InlineValueOptions,
;;; |     TextDocumentRegistrationOptions, StaticRegistrationOptions {
;;; | }

;;; | interface InlineValueParams extends WorkDoneProgressParams {
;;; |     textDocument: TextDocumentIdentifier;
;;; |     range: Range;
;;; |     context: InlineValueContext;
;;; | }

;;; | interface InlineValueContext {
;;; |     frameId: integer;
;;; |     stoppedLocation: Range;
;;; | }

;;; | interface InlineValueText {
;;; |     range: Range;
;;; |     text: string;
;;; | }

;;; | interface InlineValueVariableLookup {
;;; |     range: Range;
;;; |     variableName?: string;
;;; |     caseSensitiveLookup: boolean;
;;; | }

;;; | interface InlineValueEvaluatableExpression {
;;; |     range: Range;
;;; |     expression?: string;
;;; | }

;;; | type InlineValue = InlineValueText | InlineValueVariableLookup
;;; |     | InlineValueEvaluatableExpression;

;;; | interface InlineValueWorkspaceClientCapabilities {
;;; |     refreshSupport?: boolean;
;;; | }

;;; | interface MonikerClientCapabilities {
;;; |     dynamicRegistration?: boolean;
;;; | }

;;; | interface MonikerOptions extends WorkDoneProgressOptions {
;;; | }

;;; | interface MonikerRegistrationOptions extends
;;; |     TextDocumentRegistrationOptions, MonikerOptions {
;;; | }

;;; | interface MonikerParams extends TextDocumentPositionParams,
;;; |     WorkDoneProgressParams, PartialResultParams {
;;; | }

;;; | enum UniquenessLevel {
;;; |     document = 'document',
;;; |     project = 'project',
;;; |     group = 'group',
;;; |     scheme = 'scheme',
;;; |     global = 'global'
;;; | }

;;; | enum MonikerKind {
;;; |     import = 'import',
;;; |     = 'export',
;;; |     local = 'local'
;;; | }

;;; | interface Moniker {
;;; |     scheme: string;
;;; |     identifier: string;
;;; |     unique: UniquenessLevel;
;;; |     kind?: MonikerKind;
;;; | }

;;; | interface CompletionOptions extends WorkDoneProgressOptions {
;;; |     triggerCharacters?: string[];
;;; |     allCommitCharacters?: string[];
;;; |     resolveProvider?: boolean;
;;; |     completionItem?: {
;;; |         labelDetailsSupport?: boolean;
;;; |     }
;;; | }

(define-class completion-item-options ()
  (:label-details-support (boolean :optional t)))

(define-class completion-options (work-done-progress-options)
  (:trigger-characters (string :optional t :vector t))
  (:all-commit-characters (string :optional t :vector t))
  (:resolve-provider (boolean :optional t))
  (:completion-item (completion-item-options :optional t)))

;;; | interface CompletionRegistrationOptions
;;; |     extends TextDocumentRegistrationOptions, CompletionOptions {
;;; | }

;;; | interface CompletionParams extends TextDocumentPositionParams,
;;; |     WorkDoneProgressParams, PartialResultParams {
;;; |     context?: CompletionContext;
;;; | }

;;; | namespace CompletionTriggerKind {
;;; |     Invoked: 1 = 1;
;;; |     TriggerCharacter: 2 = 2;
;;; |     TriggerForIncompleteCompletions: 3 = 3;
;;; | }

;;; | type CompletionTriggerKind = 1 | 2 | 3;

;;; | interface CompletionContext {
;;; |     triggerKind: CompletionTriggerKind;
;;; |     triggerCharacter?: string;
;;; | }

;;; | interface CompletionList {
;;; |     isIncomplete: boolean;
;;; |     itemDefaults?: {
;;; |         commitCharacters?: string[];
;;; |         editRange?: Range | {
;;; |             insert: Range;
;;; |             replace: Range;
;;; |         };
;;; |         insertTextFormat?: InsertTextFormat;
;;; |         insertTextMode?: InsertTextMode;
;;; |         data?: LSPAny;
;;; |     }
;;; |     items: CompletionItem[];
;;; | }

;;; | namespace InsertTextFormat {
;;; |     PlainText = 1;
;;; |     Snippet = 2;
;;; | }

;;; | type InsertTextFormat = 1 | 2;

;;; | namespace CompletionItemTag {
;;; |     Deprecated = 1;
;;; | }
;;; | type CompletionItemTag = 1;

(define-enum completion-item-tag ()
  (:deprecated 1))

;;; | interface InsertReplaceEdit {
;;; |     newText: string;
;;; |     insert: Range;
;;; |     replace: Range;
;;; | }

;;; | namespace InsertTextMode {
;;; |     asIs: 1 = 1;
;;; |     adjustIndentation: 2 = 2;
;;; | }
;;; | type InsertTextMode = 1 | 2;

(define-enum insert-text-mode ()
  (:as-is 1)
  (:adjust-indentation 2))

;;; | interface CompletionItemLabelDetails {
;;; |     detail?: string;
;;; |     description?: string;
;;; | }

;;; | interface CompletionItem {
;;; |     label: string;
;;; |     labelDetails?: CompletionItemLabelDetails;
;;; |     kind?: CompletionItemKind;
;;; |     tags?: CompletionItemTag[];
;;; |     detail?: string;
;;; |     documentation?: string | MarkupContent;
;;; |     deprecated?: boolean;
;;; |     preselect?: boolean;
;;; |     sortText?: string;
;;; |     filterText?: string;
;;; |     insertText?: string;
;;; |     insertTextFormat?: InsertTextFormat;
;;; |     insertTextMode?: InsertTextMode;
;;; |     textEdit?: TextEdit | InsertReplaceEdit;
;;; |     textEditText?: string;
;;; |     additionalTextEdits?: TextEdit[];
;;; |     commitCharacters?: string[];
;;; |     command?: Command;
;;; |     data?: LSPAny;
;;; | }

;;; | interface PublishDiagnosticsClientCapabilities {
;;; |     relatedInformation?: boolean;
;;; |     tagSupport?: {
;;; |         valueSet: DiagnosticTag[];
;;; |     };
;;; |     versionSupport?: boolean;
;;; |     codeDescriptionSupport?: boolean;
;;; |     dataSupport?: boolean;
;;; | }

;;; | interface PublishDiagnosticsParams {
;;; |     uri: DocumentUri;
;;; |     version?: integer;
;;; |     diagnostics: Diagnostic[];
;;; | }

(define-class publish-diagnostics-params ()
  (:uri document-uri)
  (:version (integer :optional t))
  (:diagnostics (diagnostic :vector t)))

;;; | interface DiagnosticClientCapabilities {
;;; |     dynamicRegistration?: boolean;
;;; |     relatedDocumentSupport?: boolean;
;;; | }

;;; | interface DiagnosticOptions extends WorkDoneProgressOptions {
;;; |     identifier?: string;
;;; |     interFileDependencies: boolean;
;;; |     workspaceDiagnostics: boolean;
;;; | }

;;; | interface DiagnosticRegistrationOptions extends
;;; |     TextDocumentRegistrationOptions, DiagnosticOptions,
;;; |     StaticRegistrationOptions {
;;; | }

;;; | interface DocumentDiagnosticParams extends WorkDoneProgressParams,
;;; |     PartialResultParams {
;;; |     textDocument: TextDocumentIdentifier;
;;; |     identifier?: string;
;;; |     previousResultId?: string;
;;; | }

;;; | type DocumentDiagnosticReport = RelatedFullDocumentDiagnosticReport
;;; |     | RelatedUnchangedDocumentDiagnosticReport;

;;; | namespace DocumentDiagnosticReportKind {
;;; |     Full = 'full';
;;; |     Unchanged = 'unchanged';
;;; | }

;;; | type DocumentDiagnosticReportKind = 'full' | 'unchanged';

;;; | interface FullDocumentDiagnosticReport {
;;; |     kind: DocumentDiagnosticReportKind.Full;
;;; |     resultId?: string;
;;; |     items: Diagnostic[];
;;; | }

;;; | interface UnchangedDocumentDiagnosticReport {
;;; |     kind: DocumentDiagnosticReportKind.Unchanged;
;;; |     resultId: string;
;;; | }

;;; | interface RelatedFullDocumentDiagnosticReport extends
;;; |     FullDocumentDiagnosticReport {
;;; |     relatedDocuments?: {
;;; |         [uri: string :
;;; |         FullDocumentDiagnosticReport | UnchangedDocumentDiagnosticReport;
;;; |     };
;;; | }

;;; | interface RelatedUnchangedDocumentDiagnosticReport extends
;;; |     UnchangedDocumentDiagnosticReport {
;;; |     relatedDocuments?: {
;;; |         [uri: string :
;;; |         FullDocumentDiagnosticReport | UnchangedDocumentDiagnosticReport;
;;; |     };
;;; | }

;;; | interface DocumentDiagnosticReportPartialResult {
;;; |     relatedDocuments: {
;;; |         [uri: string :
;;; |         FullDocumentDiagnosticReport | UnchangedDocumentDiagnosticReport;
;;; |     };
;;; | }

;;; | interface DiagnosticServerCancellationData {
;;; |     retriggerRequest: boolean;
;;; | }

;;; | interface WorkspaceDiagnosticParams extends WorkDoneProgressParams,
;;; |     PartialResultParams {
;;; |     identifier?: string;
;;; |     previousResultIds: PreviousResultId[];
;;; | }

;;; | interface PreviousResultId {
;;; |     uri: DocumentUri;
;;; |     value: string;
;;; | }

;;; | interface WorkspaceDiagnosticReport {
;;; |     items: WorkspaceDocumentDiagnosticReport[];
;;; | }

;;; | interface WorkspaceFullDocumentDiagnosticReport extends
;;; |     FullDocumentDiagnosticReport {
;;; |     uri: DocumentUri;
;;; |     version: integer | null;
;;; | }

;;; | interface WorkspaceUnchangedDocumentDiagnosticReport extends
;;; |     UnchangedDocumentDiagnosticReport {
;;; |     uri: DocumentUri;
;;; |     version: integer | null;
;;; | };

;;; | type WorkspaceDocumentDiagnosticReport =
;;; |     WorkspaceFullDocumentDiagnosticReport
;;; |     | WorkspaceUnchangedDocumentDiagnosticReport;

;;; | interface WorkspaceDiagnosticReportPartialResult {
;;; |     items: WorkspaceDocumentDiagnosticReport[];
;;; | }

;;; | interface DiagnosticWorkspaceClientCapabilities {
;;; |     refreshSupport?: boolean;
;;; | }

;;; | interface SignatureHelpClientCapabilities {
;;; |     dynamicRegistration?: boolean;
;;; |     signatureInformation?: {
;;; |         documentationFormat?: MarkupKind[];
;;; |         parameterInformation?: {
;;; |             labelOffsetSupport?: boolean;
;;; |         };
;;; |         activeParameterSupport?: boolean;
;;; |     };
;;; |     contextSupport?: boolean;
;;; | }

(define-class signature-help-client-capabilities ()
  (:dynamic-registration (boolean :optional t))
  #++ (:signature-information (signature-information-options :optional t))
  (:context-support (boolean :optional t)))

;;; | interface SignatureHelpOptions extends WorkDoneProgressOptions {
;;; |     triggerCharacters?: string[];
;;; |     retriggerCharacters?: string[];
;;; | }

;;; | interface SignatureHelpRegistrationOptions
;;; |     extends TextDocumentRegistrationOptions, SignatureHelpOptions {
;;; | }

;;; | interface SignatureHelpParams extends TextDocumentPositionParams,
;;; |     WorkDoneProgressParams {
;;; |     context?: SignatureHelpContext;
;;; | }

;;; | namespace SignatureHelpTriggerKind {
;;; |     Invoked: 1 = 1;
;;; |     TriggerCharacter: 2 = 2;
;;; |     ContentChange: 3 = 3;
;;; | }

;;; | type SignatureHelpTriggerKind = 1 | 2 | 3;

;;; | interface SignatureHelpContext {
;;; |     triggerKind: SignatureHelpTriggerKind;
;;; |     triggerCharacter?: string;
;;; |     isRetrigger: boolean;
;;; |     activeSignatureHelp?: SignatureHelp;
;;; | }

;;; | interface SignatureHelp {
;;; |     signatures: SignatureInformation[];
;;; |     activeSignature?: uinteger;
;;; |     activeParameter?: uinteger;
;;; | }

;;; | interface SignatureInformation {
;;; |     label: string;
;;; |     documentation?: string | MarkupContent;
;;; |     parameters?: ParameterInformation[];
;;; |     activeParameter?: uinteger;
;;; | }

;;; | interface ParameterInformation {
;;; |     label: string | [uinteger, uinteger];
;;; |     documentation?: string | MarkupContent;
;;; | }

;;; | interface CodeActionClientCapabilities {
;;; |     dynamicRegistration?: boolean;
;;; |     codeActionLiteralSupport?: {
;;; |         codeActionKind: {
;;; |             valueSet: CodeActionKind[];
;;; |         };
;;; |     };
;;; |     isPreferredSupport?: boolean;
;;; |     disabledSupport?: boolean;
;;; |     dataSupport?: boolean;
;;; |     resolveSupport?: {
;;; |         properties: string[];
;;; |     };
;;; |     honorsChangeAnnotations?: boolean;
;;; | }

;;; | interface CodeActionOptions extends WorkDoneProgressOptions {
;;; |     codeActionKinds?: CodeActionKind[];
;;; |     resolveProvider?: boolean;
;;; | }

;;; | interface CodeActionRegistrationOptions extends
;;; |     TextDocumentRegistrationOptions, CodeActionOptions {
;;; | }

;;; | interface CodeActionParams extends WorkDoneProgressParams,
;;; |     PartialResultParams {
;;; |     textDocument: TextDocumentIdentifier;
;;; |     range: Range;
;;; |     context: CodeActionContext;
;;; | }

;;; | type CodeActionKind = string;

;;; | namespace CodeActionKind {
;;; |     Empty: CodeActionKind = '';
;;; |     QuickFix: CodeActionKind = 'quickfix';
;;; |     Refactor: CodeActionKind = 'refactor';
;;; |     RefactorExtract: CodeActionKind = 'refactor.extract';
;;; |     RefactorInline: CodeActionKind = 'refactor.inline';
;;; |     RefactorRewrite: CodeActionKind = 'refactor.rewrite';
;;; |     Source: CodeActionKind = 'source';
;;; |     SourceOrganizeImports: CodeActionKind =
;;; |         'source.organizeImports';
;;; |     SourceFixAll: CodeActionKind = 'source.fixAll';
;;; | }

;;; | interface CodeActionContext {
;;; |     diagnostics: Diagnostic[];
;;; |     only?: CodeActionKind[];
;;; |     triggerKind?: CodeActionTriggerKind;
;;; | }

;;; | namespace CodeActionTriggerKind {
;;; |     Invoked: 1 = 1;
;;; |     Automatic: 2 = 2;
;;; | }

;;; | type CodeActionTriggerKind = 1 | 2;

;;; | interface CodeAction {
;;; |     title: string;
;;; |     kind?: CodeActionKind;
;;; |     diagnostics?: Diagnostic[];
;;; |     isPreferred?: boolean;
;;; |     disabled?: {
;;; |         reason: string;
;;; |     };
;;; |     edit?: WorkspaceEdit;
;;; |     command?: Command;
;;; |     data?: LSPAny;
;;; | }

;;; | interface DocumentColorClientCapabilities {
;;; |     dynamicRegistration?: boolean;
;;; | }

;;; | interface DocumentColorOptions extends WorkDoneProgressOptions {
;;; | }

;;; | interface DocumentColorRegistrationOptions extends
;;; |     TextDocumentRegistrationOptions, StaticRegistrationOptions,
;;; |     DocumentColorOptions {
;;; | }

;;; | interface DocumentColorParams extends WorkDoneProgressParams,
;;; |     PartialResultParams {
;;; |     textDocument: TextDocumentIdentifier;
;;; | }

;;; | interface ColorInformation {
;;; |     range: Range;
;;; |     color: Color;
;;; | }

;;; | interface Color {
;;; |     readonly red: decimal;
;;; |     readonly green: decimal;
;;; |     readonly blue: decimal;
;;; |     readonly alpha: decimal;
;;; | }

;;; | interface ColorPresentationParams extends WorkDoneProgressParams,
;;; |     PartialResultParams {
;;; |     textDocument: TextDocumentIdentifier;
;;; |     color: Color;
;;; |     range: Range;
;;; | }

;;; | interface ColorPresentation {
;;; |     label: string;
;;; |     textEdit?: TextEdit;
;;; |     additionalTextEdits?: TextEdit[];
;;; | }

;;; | interface DocumentFormattingClientCapabilities {
;;; |     dynamicRegistration?: boolean;
;;; | }

;;; | interface DocumentFormattingOptions extends WorkDoneProgressOptions {
;;; | }

(define-class document-formatting-options (work-done-progress-options))

;;; | interface DocumentFormattingRegistrationOptions extends
;;; |     TextDocumentRegistrationOptions, DocumentFormattingOptions {
;;; | }

;;; | interface DocumentFormattingParams extends WorkDoneProgressParams {
;;; |     textDocument: TextDocumentIdentifier;
;;; |     options: FormattingOptions;
;;; | }

;;; | interface FormattingOptions {
;;; |     tabSize: uinteger;
;;; |     insertSpaces: boolean;
;;; |     trimTrailingWhitespace?: boolean;
;;; |     insertFinalNewline?: boolean;
;;; |     trimFinalNewlines?: boolean;
;;; |     [key: string]: boolean | integer | string;
;;; | }

;;; | interface DocumentRangeFormattingClientCapabilities {
;;; |     dynamicRegistration?: boolean;
;;; | }

;;; | interface DocumentRangeFormattingOptions extends
;;; |     WorkDoneProgressOptions {
;;; | }

;;; | interface DocumentRangeFormattingRegistrationOptions extends
;;; |     TextDocumentRegistrationOptions, DocumentRangeFormattingOptions {
;;; | }

;;; | interface DocumentRangeFormattingParams extends WorkDoneProgressParams {
;;; |     textDocument: TextDocumentIdentifier;
;;; |     range: Range;
;;; |     options: FormattingOptions;
;;; | }

;;; | interface DocumentOnTypeFormattingClientCapabilities {
;;; |     dynamicRegistration?: boolean;
;;; | }

;;; | interface DocumentOnTypeFormattingOptions {
;;; |     firstTriggerCharacter: string;
;;; |     moreTriggerCharacter?: string[];
;;; | }

;;; | interface DocumentOnTypeFormattingRegistrationOptions extends
;;; |     TextDocumentRegistrationOptions, DocumentOnTypeFormattingOptions {
;;; | }

;;; | interface DocumentOnTypeFormattingParams {
;;; |     textDocument: TextDocumentIdentifier;
;;; |     position: Position;
;;; |     ch: string;
;;; |     options: FormattingOptions;
;;; | }

;;; | namespace PrepareSupportDefaultBehavior {
;;; |     Identifier: 1 = 1;
;;; | }

;;; | type PrepareSupportDefaultBehavior = 1;

;;; | interface RenameClientCapabilities {
;;; |     dynamicRegistration?: boolean;
;;; |     prepareSupport?: boolean;
;;; |     prepareSupportDefaultBehavior?: PrepareSupportDefaultBehavior;
;;; |     honorsChangeAnnotations?: boolean;
;;; | }

;;; | interface RenameOptions extends WorkDoneProgressOptions {
;;; |     prepareProvider?: boolean;
;;; | }

;;; | interface RenameRegistrationOptions extends
;;; |     TextDocumentRegistrationOptions, RenameOptions {
;;; | }

;;; | interface RenameParams extends TextDocumentPositionParams,
;;; |     WorkDoneProgressParams {
;;; |     newName: string;
;;; | }

;;; | interface PrepareRenameParams extends TextDocumentPositionParams, WorkDoneProgressParams {
;;; | }

;;; | interface LinkedEditingRangeClientCapabilities {
;;; |     dynamicRegistration?: boolean;
;;; | }

;;; | interface LinkedEditingRangeOptions extends WorkDoneProgressOptions {
;;; | }

;;; | interface LinkedEditingRangeRegistrationOptions extends
;;; |     TextDocumentRegistrationOptions, LinkedEditingRangeOptions,
;;; |     StaticRegistrationOptions {
;;; | }

;;; | interface LinkedEditingRangeParams extends TextDocumentPositionParams,
;;; |     WorkDoneProgressParams {
;;; | }

;;; | interface LinkedEditingRanges {
;;; |     ranges: Range[];
;;; |     wordPattern?: string;
;;; | }

(define-class symbol-tag-value-set ()
  (:value-set (symbol-tag :vector t)))

(define-class resolve-support-properties ()
  (:properties (string :vector t)))

(define-class workspace-symbol-client-capabilities ()
  (:dynamic-registration (boolean :optional t))
  (:symbol-kind (symbol-kind-value-set :optional t))
  (:tag-support (symbol-tag-value-set :optional t))
  (:resolve-support (resolve-support-properties :optional t)))

;;; | interface WorkspaceSymbolOptions extends WorkDoneProgressOptions {
;;; |     resolveProvider?: boolean;
;;; | }

;;; | interface WorkspaceSymbolRegistrationOptions
;;; |     extends WorkspaceSymbolOptions {
;;; | }

;;; | interface WorkspaceSymbolParams extends WorkDoneProgressParams,
;;; |     PartialResultParams {
;;; |     query: string;
;;; | }

;;; | interface WorkspaceSymbol {
;;; |     name: string;
;;; |     kind: SymbolKind;
;;; |     tags?: SymbolTag[];
;;; |     containerName?: string;
;;; |     location: Location | { uri: DocumentUri };
;;; |     data?: LSPAny;
;;; | }

;;; | interface ConfigurationParams {
;;; |     items: ConfigurationItem[];
;;; | }

;;; | interface ConfigurationItem {
;;; |     scopeUri?: URI;
;;; |     section?: string;
;;; | }

(define-class did-change-configuration-client-capabilities ()
  (:dynamic-registration (boolean :optional t)))

(define-class did-change-configuration-params ()
  (:settings lsp-any))

;;; | interface WorkspaceFoldersServerCapabilities {
;;; |     supported?: boolean;
;;; |     changeNotifications?: string | boolean;
;;; | }

(define-class workspace-folder ()
  (:uri uri)
  (:name string))

;;; | interface DidChangeWorkspaceFoldersParams {
;;; |     event: WorkspaceFoldersChangeEvent;
;;; | }

;;; | interface WorkspaceFoldersChangeEvent {
;;; |     added: WorkspaceFolder[];
;;; |     removed: WorkspaceFolder[];
;;; | }

;;; | interface FileOperationRegistrationOptions {
;;; |     filters: FileOperationFilter[];
;;; | }

;;; | namespace FileOperationPatternKind {
;;; |     file: 'file' = 'file';
;;; |     folder: 'folder' = 'folder';
;;; | }

;;; | type FileOperationPatternKind = 'file' | 'folder';

;;; | interface FileOperationPatternOptions {
;;; |     ignoreCase?: boolean;
;;; | }

;;; | interface FileOperationPattern {
;;; |     glob: string;
;;; |     matches?: FileOperationPatternKind;
;;; |     options?: FileOperationPatternOptions;
;;; | }

;;; | interface FileOperationFilter {
;;; |     scheme?: string;
;;; |     pattern: FileOperationPattern;
;;; | }

;;; | interface CreateFilesParams {
;;; |     files: FileCreate[];
;;; | }

;;; | interface FileCreate {
;;; |     uri: string;
;;; | }

;;; | interface RenameFilesParams {
;;; |     files: FileRename[];
;;; | }

;;; | interface FileRename {
;;; |     oldUri: string;
;;; |     newUri: string;
;;; | }

;;; | interface DeleteFilesParams {
;;; |     files: FileDelete[];
;;; | }

;;; | interface FileDelete {
;;; |     uri: string;
;;; | }

(define-class did-change-watched-files-client-capabilities ()
  (:dynamic-registration (boolean :optional t))
  (:relative-pattern-support (boolean :optional t)))

;;; | interface DidChangeWatchedFilesRegistrationOptions {
;;; |     watchers: FileSystemWatcher[];
;;; | }

;;; | type Pattern = string;

;;; | interface RelativePattern {
;;; |     baseUri: WorkspaceFolder | URI;
;;; |     pattern: Pattern;
;;; | }

;;; | type GlobPattern = Pattern | RelativePattern;

;;; | interface FileSystemWatcher {
;;; |     globPattern: GlobPattern;
;;; |     kind?: WatchKind;
;;; | }

;;; | namespace WatchKind {
;;; |     Create = 1;
;;; |     Change = 2;
;;; |     Delete = 4;
;;; | }

;;; | type WatchKind = uinteger;

;;; | interface DidChangeWatchedFilesParams {
;;; |     changes: FileEvent[];
;;; | }

;;; | interface FileEvent {
;;; |     uri: DocumentUri;
;;; |     type: FileChangeType;
;;; | }

;;; | namespace FileChangeType {
;;; |     Created = 1;
;;; |     Changed = 2;
;;; |     Deleted = 3;
;;; | }

;;; | type FileChangeType = 1 | 2 | 3;

(define-class execute-command-client-capabilities ()
  (:dynamic-registration (boolean :optional t)))

;;; | interface ExecuteCommandOptions extends WorkDoneProgressOptions {
;;; |     commands: string[];
;;; | }

;;; | interface ExecuteCommandRegistrationOptions
;;; |     extends ExecuteCommandOptions {
;;; | }

;;; | interface ExecuteCommandParams extends WorkDoneProgressParams {
;;; |     command: string;
;;; |     arguments?: LSPAny[];
;;; | }

;;; | interface ApplyWorkspaceEditParams {
;;; |     label?: string;
;;; |     edit: WorkspaceEdit;
;;; | }

;;; | interface ApplyWorkspaceEditResult {
;;; |     applied: boolean;
;;; |     failureReason?: string;
;;; |     failedChange?: uinteger;
;;; | }

;;; | interface ShowMessageParams {
;;; |     type: MessageType;
;;; |     message: string;
;;; | }

;;; | namespace MessageType {
;;; |     Error = 1;
;;; |     Warning = 2;
;;; |     Info = 3;
;;; |     Log = 4;
;;; |     Debug = 5;
;;; | }

;;; | type MessageType = 1 | 2 | 3 | 4 | 5;

;;; | interface ShowMessageRequestClientCapabilities {
;;; |     messageActionItem?: {
;;; |         additionalPropertiesSupport?: boolean;
;;; |     };
;;; | }

;;; | interface ShowMessageRequestParams {
;;; |     type: MessageType;
;;; |     message: string;
;;; |     actions?: MessageActionItem[];
;;; | }

;;; | interface MessageActionItem {
;;; |     title: string;
;;; | }

;;; | interface ShowDocumentClientCapabilities {
;;; |     support: boolean;
;;; | }

;;; | interface ShowDocumentParams {
;;; |     uri: URI;
;;; |     external?: boolean;
;;; |     takeFocus?: boolean;
;;; |     selection?: Range;
;;; | }

;;; | interface ShowDocumentResult {
;;; |     success: boolean;
;;; | }

;;; | interface LogMessageParams {
;;; |     type: MessageType;
;;; |     message: string;
;;; | }

;;; | interface WorkDoneProgressCreateParams {
;;; |     token: ProgressToken;
;;; | }

;;; | interface WorkDoneProgressCancelParams {
;;; |     token: ProgressToken;
;;; | }

;;; | interface WorkspaceEditClientCapabilities {
;;; |     documentChanges?: boolean;
;;; |     resourceOperations?: ResourceOperationKind[];
;;; |     failureHandling?: FailureHandlingKind;
;;; |     normalizesLineEndings?: boolean;
;;; |     changeAnnotationSupport?: {
;;; |         groupsOnLabel?: boolean;
;;; |     };
;;; | }

(define-class change-annotation-support ()
  (:groups-on-label (boolean :optional t)))

(define-class workspace-edit-client-capabilities ()
  (:document-changes (boolean :optional t))
  (:resource-operations (resource-operation-kind :vector t))
  (:failure-handling (failure-handling-kind :optional t))
  (:normalizes-line-endings (boolean :optional t))
  (:change-annotation-support (change-annotation-support :optional t)))

(define-class text-document-sync-client-capabilities ()
  (:dynamic-registration (boolean :optional t))
  (:will-save (boolean :optional t))
  (:will-save-wait-until (boolean :optional t))
  (:did-save (boolean :optional t)))

(define-class completion-item-tag-value-set ()
  (:value-set (completion-item-tag :vector t)))

(define-class insert-text-mode-value-set ()
  (:value-set (insert-text-mode :vector t)))

(define-class completion-item-capabilities ()
  (:snippet-support (boolean :optional t))
  (:commit-characters-support (boolean :optional t))
  (:documentation-format (markup-kind :optional t :vector t))
  (:deprecated-support (boolean :optional t))
  (:preselect-support (boolean :optional t))
  (:tag-support (completion-item-tag-value-set :optional t))
  (:insert-replace-support (boolean :optional t))
  (:resolve-support (resolve-support-properties :optional t))
  (:insert-text-mode-support (insert-text-mode-value-set :optional t))
  (:label-details-support (boolean :optional t)))

(define-class completion-list-capabilities ()
  (:item-defaults (string :optional t :vector t)))

(define-enum completion-item-kind ()
  (:text 1)
  (:method 2)
  (:function 3)
  (:constructor 4)
  (:field 5)
  (:variable 6)
  (:class 7)
  (:interface 8)
  (:module 9)
  (:property 10)
  (:unit 11)
  (:value 12)
  (:enum 13)
  (:keyword 14)
  (:snippet 15)
  (:color 16)
  (:file 17)
  (:reference 18)
  (:folder 19)
  (:enum-member 20)
  (:constant 21)
  (:struct 22)
  (:event 23)
  (:operator 24)
  (:type-parameter 25))

(define-class completion-item-kind-value-set ()
  (:value-set (completion-item-kind :vector t)))

(define-class completion-client-capabilities ()
  (:dynamic-registration (boolean :optional t))
  (:completion-item completion-item-capabilities)
  (:completion-item-kind completion-item-kind-value-set)
  (:context-support (boolean :optional t))
  (:insert-text-mode (insert-text-mode :optional t))
  (:completion-list completion-list-capabilities))

;;; | interface TextDocumentClientCapabilities {
;;; |     synchronization?: TextDocumentSyncClientCapabilities;
;;; |     completion?: CompletionClientCapabilities;
;;; |     hover?: HoverClientCapabilities;
;;; |     signatureHelp?: SignatureHelpClientCapabilities;
;;; |     declaration?: DeclarationClientCapabilities;
;;; |     definition?: DefinitionClientCapabilities;
;;; |     typeDefinition?: TypeDefinitionClientCapabilities;
;;; |     implementation?: ImplementationClientCapabilities;
;;; |     references?: ReferenceClientCapabilities;
;;; |     documentHighlight?: DocumentHighlightClientCapabilities;
;;; |     documentSymbol?: DocumentSymbolClientCapabilities;
;;; |     codeAction?: CodeActionClientCapabilities;
;;; |     codeLens?: CodeLensClientCapabilities;
;;; |     documentLink?: DocumentLinkClientCapabilities;
;;; |     colorProvider?: DocumentColorClientCapabilities;
;;; |     formatting?: DocumentFormattingClientCapabilities;
;;; |     rangeFormatting?: DocumentRangeFormattingClientCapabilities;
;;; |     onTypeFormatting?: DocumentOnTypeFormattingClientCapabilities;
;;; |     rename?: RenameClientCapabilities;
;;; |     publishDiagnostics?: PublishDiagnosticsClientCapabilities;
;;; |     foldingRange?: FoldingRangeClientCapabilities;
;;; |     selectionRange?: SelectionRangeClientCapabilities;
;;; |     linkedEditingRange?: LinkedEditingRangeClientCapabilities;
;;; |     callHierarchy?: CallHierarchyClientCapabilities;
;;; |     semanticTokens?: SemanticTokensClientCapabilities;
;;; |     moniker?: MonikerClientCapabilities;
;;; |     typeHierarchy?: TypeHierarchyClientCapabilities;
;;; |     inlineValue?: InlineValueClientCapabilities;
;;; |     inlayHint?: InlayHintClientCapabilities;
;;; |     diagnostic?: DiagnosticClientCapabilities;
;;; | }

(define-class text-document-client-capabilities ()
  (:synchronization (text-document-sync-client-capabilities :optional t))
  (:completion (completion-client-capabilities :optional t))
  (:hover (hover-client-capabilities :optional t))
  (:signature-help (signature-help-client-capabilities :optional t))
  (:declaration (declaration-client-capabilities :optional t))
  (:definition (definition-client-capabilities :optional t))
  (:type-definition (type-definition-client-capabilities :optional t))
  (:implementation (implementation-client-capabilities :optional t))
  (:references (reference-client-capabilities :optional t))
  (:document-highlight (document-highlight-client-capabilities :optional t))
  (:document-symbol (document-symbol-client-capabilities :optional t))
  #++ (:code-action (code-action-client-capabilities :optional t))
  #++ (:code-lens (code-lens-client-capabilities :optional t))
  #++ (:document-link (document-link-client-capabilities :optional t))
  #++ (:color-provider (document-color-client-capabilities :optional t))
  #++ (:formatting (document-formatting-client-capabilities :optional t))
  #++ (:range-formatting (document-range-formatting-client-capabilities :optional t))
  #++ (:on-type-formatting (document-on-type-formatting-client-capabilities :optional t))
  #++ (:rename (rename-client-capabilities :optional t))
  #++ (:publish-diagnostics (publish-diagnostics-client-capabilities :optional t))
  #++ (:folding-range (folding-range-client-capabilities :optional t))
  #++ (:selection-range (selection-range-client-capabilities :optional t))
  #++ (:linked-editing-range (linked-editing-range-client-capabilities :optional t))
  #++ (:call-hierarchy (call-hierarchy-client-capabilities :optional t))
  #++ (:semantic-tokens (semantic-tokens-client-capabilities :optional t))
  #++ (:moniker (moniker-client-capabilities :optional t))
  #++ (:type-hierarchy (type-hierarchy-client-capabilities :optional t))
  #++ (:inline-value (inline-value-client-capabilities :optional t))
  #++ (:inlay-hint (inlay-hint-client-capabilities :optional t))
  #++ (:diagnostic (diagnostic-client-capabilities :optional t)))

;;; | interface NotebookDocumentClientCapabilities {
;;; |     synchronization: NotebookDocumentSyncClientCapabilities;
;;; | }

;;; | interface ClientCapabilities {
;;; |     workspace?: {
;;; |         applyEdit?: boolean;
;;; |         workspaceEdit?: WorkspaceEditClientCapabilities;
;;; |         didChangeConfiguration?: DidChangeConfigurationClientCapabilities;
;;; |         didChangeWatchedFiles?: DidChangeWatchedFilesClientCapabilities;
;;; |         symbol?: WorkspaceSymbolClientCapabilities;
;;; |         executeCommand?: ExecuteCommandClientCapabilities;
;;; |         workspaceFolders?: boolean;
;;; |         configuration?: boolean;
;;; |         semanticTokens?: SemanticTokensWorkspaceClientCapabilities;
;;; |         codeLens?: CodeLensWorkspaceClientCapabilities;
;;; |         fileOperations?: {
;;; |             dynamicRegistration?: boolean;
;;; |             didCreate?: boolean;
;;; |             willCreate?: boolean;
;;; |             didRename?: boolean;
;;; |             willRename?: boolean;
;;; |             didDelete?: boolean;
;;; |             willDelete?: boolean;
;;; |         };
;;; |         inlineValue?: InlineValueWorkspaceClientCapabilities;
;;; |         inlayHint?: InlayHintWorkspaceClientCapabilities;
;;; |         diagnostics?: DiagnosticWorkspaceClientCapabilities;
;;; |     };
;;; |     textDocument?: TextDocumentClientCapabilities;
;;; |     notebookDocument?: NotebookDocumentClientCapabilities;
;;; |     window?: {
;;; |         workDoneProgress?: boolean;
;;; |         showMessage?: ShowMessageRequestClientCapabilities;
;;; |         showDocument?: ShowDocumentClientCapabilities;
;;; |     };
;;; |     general?: {
;;; |         staleRequestSupport?: {
;;; |             cancel: boolean;
;;; |             retryOnContentModified: string[];
;;; |         }
;;; |         regularExpressions?: RegularExpressionsClientCapabilities;
;;; |         markdown?: MarkdownClientCapabilities;
;;; |         positionEncodings?: PositionEncodingKind[];
;;; |     };
;;; |     experimental?: LSPAny;
;;; | }

(define-class workspace-client-capabilities ()
  (:apply-edit (boolean :optional t))
  (:workspace-edit (workspace-edit-client-capabilities :optional t))
  (:did-change-configuration (did-change-configuration-client-capabilities :optional t))
  (:did-change-watched-files (did-change-watched-files-client-capabilities :optional t))
  (:symbol (workspace-symbol-client-capabilities :optional t))
  (:execute-command (execute-command-client-capabilities :optional t))
  (:workspace-folders (boolean :optional t))
  (:configuration (boolean :optional t))
  (:semantic-tokens (semantic-tokens-workspace-client-capabilities :optional t))
  (:code-lens (code-lens-workspace-client-capabilities :optional t))
  #++ (:file-operations (file-operations-client-capabilities :optional t))
  #++ (:inline-value (inline-value-workspace-client-capabilities :optional t))
  #++ (:inlay-hint (inlay-hint-workspace-client-capabilities :optional t))
  #++ (:diagnostics (diagnostic-workspace-client-capabilities :optional t)))

(define-class client-capabilities ()
  (:workspace (workspace-client-capabilities :optional t))
  (:text-document (text-document-client-capabilities :optional t))
  #++ (:notebook-document (notebook-document-client-capabilities :optional t))
  #++ (:window (window-client-capabilities :optional t))
  #++ (:general (general-client-capabilities :optional t))
  #++ (:experimental lsp-any))

;;; | interface ServerCapabilities {
;;; |     positionEncoding?: PositionEncodingKind;
;;; |     textDocumentSync?: TextDocumentSyncOptions | TextDocumentSyncKind;
;;; |     notebookDocumentSync?: NotebookDocumentSyncOptions
;;; |     | NotebookDocumentSyncRegistrationOptions;
;;; |     completionProvider?: CompletionOptions;
;;; |     hoverProvider?: boolean | HoverOptions;
;;; |     signatureHelpProvider?: SignatureHelpOptions;
;;; |     declarationProvider?: boolean | DeclarationOptions
;;; |     | DeclarationRegistrationOptions;
;;; |     definitionProvider?: boolean | DefinitionOptions;
;;; |     typeDefinitionProvider?: boolean | TypeDefinitionOptions
;;; |     | TypeDefinitionRegistrationOptions;
;;; |     implementationProvider?: boolean | ImplementationOptions
;;; |     | ImplementationRegistrationOptions;
;;; |     referencesProvider?: boolean | ReferenceOptions;
;;; |     documentHighlightProvider?: boolean | DocumentHighlightOptions;
;;; |     documentSymbolProvider?: boolean | DocumentSymbolOptions;
;;; |     codeActionProvider?: boolean | CodeActionOptions;
;;; |     codeLensProvider?: CodeLensOptions;
;;; |     documentLinkProvider?: DocumentLinkOptions;
;;; |     colorProvider?: boolean | DocumentColorOptions
;;; |     | DocumentColorRegistrationOptions;
;;; |     documentFormattingProvider?: boolean | DocumentFormattingOptions;
;;; |     documentRangeFormattingProvider?: boolean | DocumentRangeFormattingOptions;
;;; |     documentOnTypeFormattingProvider?: DocumentOnTypeFormattingOptions;
;;; |     renameProvider?: boolean | RenameOptions;
;;; |     foldingRangeProvider?: boolean | FoldingRangeOptions
;;; |     | FoldingRangeRegistrationOptions;
;;; |     executeCommandProvider?: ExecuteCommandOptions;
;;; |     selectionRangeProvider?: boolean | SelectionRangeOptions
;;; |     | SelectionRangeRegistrationOptions;
;;; |     linkedEditingRangeProvider?: boolean | LinkedEditingRangeOptions
;;; |     | LinkedEditingRangeRegistrationOptions;
;;; |     callHierarchyProvider?: boolean | CallHierarchyOptions
;;; |     | CallHierarchyRegistrationOptions;
;;; |     semanticTokensProvider?: SemanticTokensOptions | SemanticTokensRegistrationOptions;
;;; |     monikerProvider?: boolean | MonikerOptions | MonikerRegistrationOptions;
;;; |     typeHierarchyProvider?: boolean | TypeHierarchyOptions | TypeHierarchyRegistrationOptions;
;;; |     inlineValueProvider?: boolean | InlineValueOptions | InlineValueRegistrationOptions;
;;; |     inlayHintProvider?: boolean | InlayHintOptions | InlayHintRegistrationOptions;
;;; |     diagnosticProvider?: DiagnosticOptions | DiagnosticRegistrationOptions;
;;; |     workspaceSymbolProvider?: boolean | WorkspaceSymbolOptions;
;;; |     workspace?: {
;;; |         workspaceFolders?: WorkspaceFoldersServerCapabilities;
;;; |         fileOperations?: {
;;; |             didCreate?: FileOperationRegistrationOptions;
;;; |             willCreate?: FileOperationRegistrationOptions;
;;; |             didRename?: FileOperationRegistrationOptions;
;;; |             willRename?: FileOperationRegistrationOptions;
;;; |             didDelete?: FileOperationRegistrationOptions;
;;; |             willDelete?: FileOperationRegistrationOptions;
;;; |         };
;;; |     };
;;; |     experimental?: LSPAny;
;;; | }

(define-class client-info ()
  (:name string)
  (:version (string :optional t)))

(define-class initialize-params (work-done-progress-params)
  (:process-id (integer :optional t))
  (:client-info (client-info :optional t))
  (:locale (string :optional t))
  (:root-path (string :optional t))
  (:root-uri (uri :optional t))
  (:initialization-options (lsp-any :optional t))
  (:capabilities client-capabilities)
  (:trace (trace-value :optional t))
  (:workspace-folders (workspace-folder :vector t :optional t)))

(define-union document-symbol-provider-options
  (boolean document-symbol-options))

(define-class server-capabilities ()
  (:position-encoding (position-encoding-kind :optional t))
  (:text-document-sync (text-document-sync-options :optional t))
  #++ (:notebook-document-sync (or notebook-document-sync-options notebook-document-sync-registration-options))
  (:completion-provider (completion-options :optional t))
  #++ (:hover-provider (or boolean hover-options))
  #++ (:signature-help-provider (signature-help-options :optional t))
  #++ (:declaration-provider (declaration-options declaration-registration-options))
  (:definition-provider (definition-options :optional t))
  #++ (:type-definition-provider (or boolean type-definition-options type-definition-registration-options))
  #++ (:implementation-provider (or boolean implementation-options implementation-registration-options))
  #++ (:references-provider (or boolean reference-options))
  #++ (:document-highlight-provider (or boolean document-highlight-options))
  (:document-symbol-provider (document-symbol-provider-options :optional t))
  #++ (:code-action-provider (or boolean code-action-options))
  #++ (:code-lens-provider (code-lens-options :optional t))
  (:document-link-provider (document-link-options :optional t))
  #++ (:color-provider (or boolean document-color-options document-color-registration-options))
  (:document-formatting-provider (document-formatting-options :optional t))
  #++ (:document-range-formatting-provider (or boolean document-range-formatting-options))
  #++ (:document-on-type-formatting-provider (document-on-type-formatting-options :optional t))
  #++ (:rename-provider (or boolean rename-options))
  #++ (:folding-range-provider (or boolean folding-range-options folding-range-registration-options))
  #++ (:execute-command-provider (execute-command-options :optional t))
  #++ (:selection-range-provider (or boolean selection-range-options selection-range-registration-options))
  #++ (:linked-editing-range-provider (or boolean linked-editing-range-options linked-editing-range-registration-options))
  #++ (:call-hierarchy-provider (or boolean call-hierarchy-options call-hierarchy-registration-options))
  (:semantic-tokens-provider semantic-tokens-registration-options)
  #++ (:moniker-provider (or boolean moniker-options moniker-registration-options))
  #++ (:type-hierarchy-provider (or boolean type-hierarchy-options type-hierarchy-registration-options))
  #++ (:inline-value-provider (or boolean inline-value-options inline-value-registration-options))
  #++ (:inlay-hint-provider (or boolean inlay-hint-options inlay-hint-registration-options))
  #++ (:diagnostic-provider (or diagnostic-options diagnostic-registration-options))
  #++ (:workspace-symbol-provider (boolean workspace-symbol-options))
  #++ (:workspace (workspace-server-capabilities :optional t))
  (:experimental (lsp-any :optional t)))

(define-class server-info ()
  (:name string)
  (:version (string :optional t)))

(define-class initialize-result ()
  (:capabilities server-capabilities)
  (:server-info server-info))

;;; | namespace InitializeErrorCodes {
;;; |     unknownProtocolVersion: 1 = 1;
;;; | }

;;; | type InitializeErrorCodes = 1;

;;; | interface InitializeError {
;;; |     retry: boolean;
;;; | }

(define-class initialized-params ())
