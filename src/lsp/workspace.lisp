;;;; LSP request handlers

(in-package #:coalton-lsp)

;;; request: workspace/applyEdit

(defun handle-workspace-apply-edit (session request)
  (declare (ignore session request))
  (error "writeme: handler workspace/applyEdit"))

;;; request: workspace/codeLens-refresh

(defun handle-workspace-code-lens-refresh (session request)
  (declare (ignore session request))
  (error "writeme: handler workspace/codeLens-refresh"))

;;; request: workspace/configuration

(defun handle-workspace-configuration (session request)
  (declare (ignore session request))
  (error "writeme: handler workspace/configuration"))

;;; request: workspace/diagnostic

(defun handle-workspace-diagnostic (session request)
  (declare (ignore session request))
  (error "writeme: handler workspace/diagnostic"))

;;; request: workspace/diagnostic/refresh

(defun handle-workspace-diagnostic-refresh (session request)
  (declare (ignore session request))
  (error "writeme: handler workspace/diagnostic/refresh"))

;;; request: workspace/didChangeConfiguration

(defun handle-workspace-did-change-configuration (session request)
  (update-configuration session (get-field (request-params request) :settings)))

;;; request: workspace/didChangeWatchedFiles

(defun handle-workspace-did-change-watched-files (session request)
  (declare (ignore session request))
  (error "writeme: handler workspace/didChangeWatchedFiles"))

;;; request: workspace/didChangeWorkspaceFolders

(defun handle-workspace-did-change-workspace-folders (session request)
  (declare (ignore session request))
  (error "writeme: handler workspace/didChangeWorkspaceFolders"))

;;; request: workspace/didCreateFiles

(defun handle-workspace-did-create-files (session request)
  (declare (ignore session request))
  (error "writeme: handler workspace/didCreateFiles"))

;;; request: workspace/didDeleteFiles

(defun handle-workspace-did-delete-files (session request)
  (declare (ignore session request))
  (error "writeme: handler workspace/didDeleteFiles"))

;;; request: workspace/didRenameFiles

(defun handle-workspace-did-rename-files (session request)
  (declare (ignore session request))
  (error "writeme: handler workspace/didRenameFiles"))

;;; request: workspace/executeCommand

(defun handle-workspace-execute-command (session request)
  (declare (ignore session request))
  (error "writeme: handler workspace/executeCommand"))

;;; request: workspace/inlayHint/refresh

(defun handle-workspace-inlay-hint-refresh (session request)
  (declare (ignore session request))
  (error "writeme: handler workspace/inlayHint/refresh"))

;;; request: workspace/inlineValue/refresh

(defun handle-workspace-inline-value-refresh (session request)
  (declare (ignore session request))
  (error "writeme: handler workspace/inlineValue/refresh"))

;;; request: workspace/semanticTokens/refresh

(defun handle-workspace-semantic-tokens-refresh (session request)
  (declare (ignore session request))
  (error "writeme: handler workspace/semanticTokens/refresh"))

;;; request: workspace/symbol

(defun handle-workspace-symbol (session request)
  (declare (ignore session request))
  (error "writeme: handler workspace/symbol"))

;;; request: workspace/willCreateFiles

(defun handle-workspace-will-create-files (session request)
  (declare (ignore session request))
  (error "writeme: handler workspace/willCreateFiles"))

;;; request: workspace/willDeleteFiles

(defun handle-workspace-will-delete-files (session request)
  (declare (ignore session request))
  (error "writeme: handler workspace/willDeleteFiles"))

;;; request: workspace/willRenameFiles

(defun handle-workspace-will-rename-files (session request)
  (declare (ignore session request))
  (error "writeme: handler workspace/willRenameFiles"))

;;; request: workspace/workspaceFolders

(defun handle-workspace-workspace-folders (session request)
  (declare (ignore session request))
  (error "writeme: handler workspace/workspaceFolders"))

;;; request: workspaceSymbol/resolve

(defun handle-workspace-symbol-resolve (session request)
  (declare (ignore session request))
  (error "writeme: handler workspaceSymbol/resolve"))
