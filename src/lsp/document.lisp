;;;; LSP requests related to test documents

(in-package #:coalton-lsp)

;;; request: textDocument/codeAction

(defun handle-text-document-code-action (session request)
  (declare (ignore session request))
  (error "writeme: handler textDocument/codeAction"))

;;; request: textDocument/codeLens

(defun handle-text-document-code-lens (session request)
  (declare (ignore session request))
  (error "writeme: handler textDocument/codeLens"))

;;; request: textDocument/colorPresentation

(defun handle-text-document-color-presentation (session request)
  (declare (ignore session request))
  (error "writeme: handler textDocument/colorPresentation"))

;;; request: textDocument/completion

(defun handle-text-document-completion (session request)
  (declare (ignore session request))
  (error "writeme: handler textDocument/completion"))

;;; request: textDocument/declaration

(defun handle-text-document-declaration (session request)
  (declare (ignore session request))
  (error "writeme: handler textDocument/declaration"))

;;; request: textDocument/definition

(defun handle-text-document-definition (session request)
  (declare (ignore session request))
  (error "writeme: handler textDocument/definition"))

;;; request: textDocument/diagnostic

(defun handle-text-document-diagnostic (session request)
  (declare (ignore session request))
  (error "writeme: handler textDocument/diagnostic"))

;;; request: textDocument/didChange

(defun handle-text-document-did-change (session request)
  (change-document session (get-field (request-params request) :text-document)))

;;; request: textDocument/didClose

(defun handle-text-document-did-close (session request)
  (declare (ignore session request))
  (error "writeme: handler textDocument/didClose"))

;;; request: textDocument/didOpen

(defun handle-text-document-did-open (session request)
  (open-document session (get-field (request-params request) :text-document)))

;;; request: textDocument/didSave

(defun handle-text-document-did-save (session request)
  (declare (ignore session request))
  (error "writeme: handler textDocument/didSave"))

;;; request: textDocument/documentColor

(defun handle-text-document-document-color (session request)
  (declare (ignore session request))
  (error "writeme: handler textDocument/documentColor"))

;;; request: textDocument/documentHighlight

(defun handle-text-document-document-highlight (session request)
  (declare (ignore session request))
  (error "writeme: handler textDocument/documentHighlight"))

;;; request: textDocument/documentLink

(defun handle-text-document-document-link (session request)
  (declare (ignore session request))
  (error "writeme: handler textDocument/documentLink"))

;;; request: textDocument/documentSymbol

(defun handle-text-document-document-symbol (session request)
  (make-vector-response (get-field request :id)
                        (document-symbols (get-field (request-params request)
                                                     (list :text-document :uri)))))

;;; request: textDocument/foldingRange

(defun handle-text-document-folding-range (session request)
  (declare (ignore session request))
  (error "writeme: handler textDocument/foldingRange"))

;;; request: textDocument/formatting

(defun handle-text-document-formatting (session request)
  (declare (ignore session request))
  (error "writeme: handler textDocument/formatting"))

;;; request: textDocument/hover

(defun handle-text-document-hover (session request)
  (declare (ignore session request))
  (error "writeme: handler textDocument/hover"))

;;; request: textDocument/implementation

(defun handle-text-document-implementation (session request)
  (declare (ignore session request))
  (error "writeme: handler textDocument/implementation"))

;;; request: textDocument/inlayHint

(defun handle-text-document-inlay-hint (session request)
  (declare (ignore session request))
  (error "writeme: handler textDocument/inlayHint"))

;;; request: textDocument/inlineValue

(defun handle-text-document-inline-value (session request)
  (declare (ignore session request))
  (error "writeme: handler textDocument/inlineValue"))

;;; request: textDocument/linkedEditingRange

(defun handle-text-document-linked-editing-range (session request)
  (declare (ignore session request))
  (error "writeme: handler textDocument/linkedEditingRange"))

;;; request: textDocument/moniker

(defun handle-text-document-moniker (session request)
  (declare (ignore session request))
  (error "writeme: handler textDocument/moniker"))

;;; request: textDocument/onTypeFormatting

(defun handle-text-document-on-type-formatting (session request)
  (declare (ignore session request))
  (error "writeme: handler textDocument/onTypeFormatting"))

;;; request: textDocument/prepareCallHierarchy

(defun handle-text-document-prepare-call-hierarchy (session request)
  (declare (ignore session request))
  (error "writeme: handler textDocument/prepareCallHierarchy"))

;;; request: textDocument/prepareRename

(defun handle-text-document-prepare-rename (session request)
  (declare (ignore session request))
  (error "writeme: handler textDocument/prepareRename"))

;;; request: textDocument/prepareTypeHierarchy

(defun handle-text-document-prepare-type-hierarchy (session request)
  (declare (ignore session request))
  (error "writeme: handler textDocument/prepareTypeHierarchy"))

;;; request: textDocument/publishDiagnostics

(defun handle-text-document-publish-diagnostics (session request)
  (declare (ignore session request))
  (error "writeme: handler textDocument/publishDiagnostics"))

;;; request: textDocument/rangeFormatting

(defun handle-text-document-range-formatting (session request)
  (declare (ignore session request))
  (error "writeme: handler textDocument/rangeFormatting"))

;;; request: textDocument/references

(defun handle-text-document-references (session request)
  (declare (ignore session request))
  (error "writeme: handler textDocument/references"))

;;; request: textDocument/rename

(defun handle-text-document-rename (session request)
  (declare (ignore session request))
  (error "writeme: handler textDocument/rename"))

;;; request: textDocument/selectionRange

(defun handle-text-document-selection-range (session request)
  (declare (ignore session request))
  (error "writeme: handler textDocument/selectionRange"))

;;; request: textDocument/semanticTokens/full

(defun handle-text-document-semantic-tokens-full (session request)
  (declare (ignore session request))
  (error "writeme: handler textDocument/semanticTokens/full"))

;;; request: textDocument/semanticTokens/full/delta

(defun handle-text-document-semantic-tokens-full-delta (session request)
  (declare (ignore session request))
  (error "writeme: handler textDocument/semanticTokens/full/delta"))

;;; request: textDocument/semanticTokens/range

(defun handle-text-document-semantic-tokens-range (session request)
  ;;(session-semantic-tokens-range session (message-value request))
  (let ((result (make-message 'semantic-tokens)))
    (set-field result (list :data)
               (list 0 9 9 0 0))
    (make-response (get-field request :id)
                   result)))

;;; request: textDocument/signatureHelp

(defun handle-text-document-signature-help (session request)
  (declare (ignore session request))
  (error "writeme: handler textDocument/signatureHelp"))

;;; request: textDocument/typeDefinition

(defun handle-text-document-type-definition (session request)
  (declare (ignore session request))
  (error "writeme: handler textDocument/typeDefinition"))

;;; request: textDocument/willSave

(defun handle-text-document-will-save (session request)
  (declare (ignore session request))
  (error "writeme: handler textDocument/willSave"))

;;; request: textDocument/willSaveWaitUntil

(defun handle-text-document-will-save-wait-until (session request)
  (declare (ignore session request))
  (error "writeme: handler textDocument/willSaveWaitUntil"))
