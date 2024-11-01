;;;; LSP request handlers

(in-package #:coalton-lsp)

;;; request: $/logTrace

(define-handler "$/logTrace"
  log-trace-params
  handle-log-trace)

;;; request: $/setTrace

(define-handler "$/setTrace"
  set-trace-params
  handle-set-trace)

;;; request: callHierarchy/incomingCalls

(define-handler "callHierarchy/incomingCalls"
  call-hierarchy-incoming-calls-params
  handle-call-hierarchy-incoming-calls)

;;; request: callHierarchy/outgoingCalls

(define-handler "callHierarchy/outgoingCalls"
  call-hierarchy-outgoing-calls-params
  handle-call-hierarchy-outgoing-calls)

;;; request: client/registerCapability

(define-handler "client/registerCapability"
  registration-params
  handle-client-register-capability)

;;; request: client/unregisterCapability

(define-handler "client/unregisterCapability"
  unregistration-params
  handle-client-unregister-capability)

;;; request: codeAction/resolve

(define-handler "codeAction/resolve"
  code-action
  handle-code-action-resolve)

;;; request: codeLens/resolve

(define-handler "codeLens/resolve"
  code-lens
  handle-code-lens-resolve)

;;; request: completionItem/resolve

(define-handler "completionItem/resolve"
  completion-item
  handle-completion-item-resolve)

;;; request: documentLink/resolve

(define-handler "documentLink/resolve"
  document-link
  handle-document-link-resolve)

;;; request: exit

(define-handler "exit"
  empty
  handle-exit)

;;; request: initialize

;;; TODO make a less fugly version of set-field

(defun handle-initialize (session request)
  (initializing-session session (message-value (request-params request)))
  (let ((result (make-message 'initialize-result)))
    (set-field result (list :server-info :name) "Coalton")
    (set-field result (list :capabilities :text-document-sync :open-close) t)
    (set-field result (list :capabilities :text-document-sync :change) :full)
    (set-field result (list :capabilities :definition-provider :work-done-progress) t)
    (set-field result (list :capabilities :document-formatting-provider :work-done-progress) t)
    (set-field result (list :capabilities :document-symbol-provider) t)
    (set-field result (list :capabilities :semantic-tokens-provider :legend :token-types)
               '("namespace" "type" "function" "macro" "keyword" "class" "variable" "method"
                 "event" "interface"))
    (set-field result (list :capabilities :semantic-tokens-provider :legend :token-modifiers)
               '("definition" "defaultLibrary" "implementation"))
    (set-field result (list :capabilities :semantic-tokens-provider :range) t)
    (set-field result (list :capabilities :semantic-tokens-provider :full) t)
    (set-field result (list :capabilities :position-encoding)
               (position-encoding session))
    (make-response (get-field request :id)
                   result)))

;;; request: initialize

(define-handler "initialize"
  initialize-params
  handle-initialize)

;;; request: initialized

(defun handle-initialized (session request)
  (declare (ignore request))
  (initialized-session session))

(define-handler "initialized"
  initialized-params
  handle-initialized)

;;; request: inlayHint/resolve

(define-handler "inlayHint/resolve"
  inlay-hint
  handle-inlay-hint-resolve)

;;; request: notebookDocument/didChange

(define-handler "notebookDocument/didChange"
  did-change-notebook-document-params
  handle-notebook-document-did-change)

;;; request: notebookDocument/didClose

(define-handler "notebookDocument/didClose"
  did-close-notebook-document-params
  handle-notebook-document-did-close)

;;; request: notebookDocument/didOpen

(define-handler "notebookDocument/didOpen"
  did-open-notebook-document-params
  handle-notebook-document-did-open)

;;; request: notebookDocument/didSave

(define-handler "notebookDocument/didSave"
  did-save-notebook-document-params
  handle-notebook-document-did-save)

;;; request: shutdown

(defun handle-shutdown (session request)
  (shutdown-session session)
  (make-response (get-field request :id)
                 (make-message 'empty)))

(define-handler "shutdown"
  empty
  handle-shutdown)

;;; request: telemetry/event

(define-handler "telemetry/event"
  lsp-any
  handle-telemetry-event)

;;; request: textDocument/codeAction

(define-handler "textDocument/codeAction"
  code-action-params
  handle-text-document-code-action)

;;; request: textDocument/codeLens

(define-handler "textDocument/codeLens"
  code-lens-params
  handle-text-document-code-lens)

;;; request: textDocument/colorPresentation

(define-handler "textDocument/colorPresentation"
  color-presentation-params
  handle-text-document-color-presentation)

;;; request: textDocument/completion

(define-handler "textDocument/completion"
  completion-params
  handle-text-document-completion)

;;; request: textDocument/declaration

(define-handler "textDocument/declaration"
  declaration-params
  handle-text-document-declaration)

;;; request: textDocument/definition

(define-handler "textDocument/definition"
  definition-params
  handle-text-document-definition)

;;; request: textDocument/diagnostic

(define-handler "textDocument/diagnostic"
  document-diagnostic-params
  handle-text-document-diagnostic)

;;; request: textDocument/didChange

(defun handle-text-document-did-change (session request)
  (change-document session (get-field (request-params request) :text-document)))

(define-handler "textDocument/didChange"
  did-change-text-document-params
  handle-text-document-did-change)

;;; request: textDocument/didClose

(define-handler "textDocument/didClose"
  did-close-text-document-params
  handle-text-document-did-close)

;;; request: textDocument/didOpen

(defun handle-text-document-did-open (session request)
  (open-document session (get-field (request-params request) :text-document)))

(define-handler "textDocument/didOpen"
  did-open-text-document-params
  handle-text-document-did-open)

;;; request: textDocument/didSave

(define-handler "textDocument/didSave"
  did-save-text-document-params
  handle-text-document-did-save)

;;; request: textDocument/documentColor

(define-handler "textDocument/documentColor"
  document-color-params
  handle-text-document-document-color)

;;; request: textDocument/documentHighlight

(define-handler "textDocument/documentHighlight"
  document-highlight-params
  handle-text-document-document-highlight)

;;; request: textDocument/documentLink

(define-handler "textDocument/documentLink"
  document-link-params
  handle-text-document-document-link)

;;; request: textDocument/documentSymbol

(defun handle-text-document-document-symbol (session request)
  (make-vector-response (get-field request :id)
                        (document-symbols (get-field (request-params request)
                                                     (list :text-document :uri)))))

(define-handler "textDocument/documentSymbol"
  document-symbol-params
  handle-text-document-document-symbol)

;;; request: textDocument/foldingRange

(define-handler "textDocument/foldingRange"
  folding-range-params
  handle-text-document-folding-range)

;;; request: textDocument/formatting

(define-handler "textDocument/formatting"
  document-formatting-params
  handle-text-document-formatting)

;;; request: textDocument/hover

(define-handler "textDocument/hover"
  hover-params
  handle-text-document-hover)

;;; request: textDocument/implementation

(define-handler "textDocument/implementation"
  implementation-params
  handle-text-document-implementation)

;;; request: textDocument/inlayHint

(define-handler "textDocument/inlayHint"
  inlay-hint-params
  handle-text-document-inlay-hint)

;;; request: textDocument/inlineValue

(define-handler "textDocument/inlineValue"
  inline-value-params
  handle-text-document-inline-value)

;;; request: textDocument/linkedEditingRange

(define-handler "textDocument/linkedEditingRange"
  linked-editing-range-params
  handle-text-document-linked-editing-range)

;;; request: textDocument/moniker

(define-handler "textDocument/moniker"
  moniker-params
  handle-text-document-moniker)

;;; request: textDocument/onTypeFormatting

(define-handler "textDocument/onTypeFormatting"
  document-on-type-formatting-params
  handle-text-document-on-type-formatting)

;;; request: textDocument/prepareCallHierarchy

(define-handler "textDocument/prepareCallHierarchy"
  call-hierarchy-prepare-params
  handle-text-document-prepare-call-hierarchy)

;;; request: textDocument/prepareRename

(define-handler "textDocument/prepareRename"
  prepare-rename-params
  handle-text-document-prepare-rename)

;;; request: textDocument/prepareTypeHierarchy

(define-handler "textDocument/prepareTypeHierarchy"
  type-hierarchy-prepare-params
  handle-text-document-prepare-type-hierarchy)

;;; request: textDocument/publishDiagnostics

(define-handler "textDocument/publishDiagnostics"
  publish-diagnostics-params
  handle-text-document-publish-diagnostics)

;;; request: textDocument/rangeFormatting

(define-handler "textDocument/rangeFormatting"
  document-range-formatting-params
  handle-text-document-range-formatting)

;;; request: textDocument/references

(define-handler "textDocument/references"
  reference-params
  handle-text-document-references)

;;; request: textDocument/rename

(define-handler "textDocument/rename"
  rename-params
  handle-text-document-rename)

;;; request: textDocument/selectionRange

(define-handler "textDocument/selectionRange"
  selection-range-params
  handle-text-document-selection-range)

;;; request: textDocument/semanticTokens/full

(define-handler "textDocument/semanticTokens/full"
  semantic-tokens-params
  handle-text-document-semantic-tokens-full)

;;; request: textDocument/semanticTokens/full/delta

(define-handler "textDocument/semanticTokens/full/delta"
  semantic-tokensDelta-params
  handle-text-document-semantic-tokens-full-delta)

;;; request: textDocument/semanticTokens/range

(defun handle-text-document-semantic-tokens-range (session request)
  ;;(session-semantic-tokens-range session (message-value request))
  (let ((result (make-message 'semantic-tokens)))
    (set-field result (list :data)
               (list 0 9 9 0 0))
    (make-response (get-field request :id)
                   result)))

;;; request: textDocument/semanticTokens/range

(define-handler "textDocument/semanticTokens/range"
  semantic-tokens-range-params
  handle-text-document-semantic-tokens-range)

;;; request: textDocument/signatureHelp

(define-handler "textDocument/signatureHelp"
  signature-help-params
  handle-text-document-signature-help)

;;; request: textDocument/typeDefinition

(define-handler "textDocument/typeDefinition"
  type-definition-params
  handle-text-document-type-definition)

;;; request: textDocument/willSave

(define-handler "textDocument/willSave"
  will-save-text-document-params
  handle-text-document-will-save)

;;; request: textDocument/willSaveWaitUntil

(define-handler "textDocument/willSaveWaitUntil"
  will-save-text-document-params
  handle-text-document-will-save-wait-until)

;;; request: typeHierarchy/subtypes

(define-handler "typeHierarchy/subtypes"
  type-hierarchy-subtypes-params
  handle-type-hierarchy-subtypes)

;;; request: typeHierarchy/supertypes

(define-handler "typeHierarchy/supertypes"
  type-hierarchy-supertypes-params
  handle-type-hierarchy-supertypes)

;;; request: window/logMessage

(define-handler "window/logMessage"
  log-message-params
  handle-window-log-message)

;;; request: window/showDocument

(define-handler "window/showDocument"
  show-document-params
  handle-window-show-document)

;;; request: window/showMessage

(define-handler "window/showMessage"
  show-message-params
  handle-window-show-message)

;;; request: window/showMessageRequest

(define-handler "window/showMessageRequest"
  show-message-request-params
  handle-window-show-message-request)

;;; request: window/workDoneProgress/cancel

(define-handler "window/workDoneProgress/cancel"
  work-done-progress-cancel-params
  handle-window-work-done-progress-cancel)

;;; request: window/workDoneProgress/create

(define-handler "window/workDoneProgress/create"
  work-done-progress-create-params
  handle-window-work-done-progress-create)

;;; request: workspace/applyEdit

(define-handler "workspace/applyEdit"
  apply-workspace-edit-params
  handle-workspace-apply-edit)

;;; request: workspace/codeLens-refresh

(define-handler "workspace/codeLens-refresh"
  empty
  handle-workspace-code-lens-refresh)

;;; request: workspace/configuration

(define-handler "workspace/configuration"
  configuration-params
  handle-workspace-configuration)

;;; request: workspace/diagnostic

(define-handler "workspace/diagnostic"
  workspace-diagnostic-params
  handle-workspace-diagnostic)

;;; request: workspace/diagnostic/refresh

(define-handler "workspace/diagnostic/refresh"
  empty
  handle-workspace-diagnostic-refresh)

;;; request: workspace/didChangeConfiguration

(defun handle-workspace-did-change-configuration (session request)
  (update-configuration session (get-field (request-params request) :settings)))

(define-handler "workspace/didChangeConfiguration"
  did-change-configuration-params
  handle-workspace-did-change-configuration)

;;; request: workspace/didChangeWatchedFiles

(define-handler "workspace/didChangeWatchedFiles"
  did-change-watched-files-params
  handle-workspace-did-change-watched-files)

;;; request: workspace/didChangeWorkspaceFolders

(define-handler "workspace/didChangeWorkspaceFolders"
  did-change-workspace-folders-params
  handle-workspace-did-change-workspace-folders)

;;; request: workspace/didCreateFiles

(define-handler "workspace/didCreateFiles"
  create-files-params
  handle-workspace-didCreate-files)

;;; request: workspace/didDeleteFiles

(define-handler "workspace/didDeleteFiles"
  delete-files-params
  handle-workspace-did-delete-files)

;;; request: workspace/didRenameFiles

(define-handler "workspace/didRenameFiles"
  rename-files-params
  handle-workspace-did-rename-files)

;;; request: workspace/executeCommand

(define-handler "workspace/executeCommand"
  execute-command-params
  handle-workspace-execute-command)

;;; request: workspace/inlayHint/refresh

(define-handler "workspace/inlayHint/refresh"
  empty
  handle-workspace-inlay-hint-refresh)

;;; request: workspace/inlineValue/refresh

(define-handler "workspace/inlineValue/refresh"
  empty
  handle-workspace-inline-value-refresh)

;;; request: workspace/semanticTokens/refresh

(define-handler "workspace/semanticTokens/refresh"
  empty
  handle-workspace-semantic-tokens-refresh)

;;; request: workspace/symbol

(define-handler "workspace/symbol"
  workspace-symbol-params
  handle-workspace-symbol)

;;; request: workspace/willCreateFiles

(define-handler "workspace/willCreateFiles"
  create-files-params
  handle-workspace-willCreate-files)

;;; request: workspace/willDeleteFiles

(define-handler "workspace/willDeleteFiles"
  delete-files-params
  handle-workspace-will-delete-files)

;;; request: workspace/willRenameFiles

(define-handler "workspace/willRenameFiles"
  rename-files-params
  handle-workspace-will-rename-files)

;;; request: workspace/workspaceFolders

(define-handler "workspace/workspaceFolders"
  empty
  handle-workspace-workspace-folders)

;;; request: workspaceSymbol/resolve

(define-handler "workspaceSymbol/resolve"
  workspace-symbol
  handle-workspace-symbol-resolve)
