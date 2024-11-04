;;;; LSP request handlers

(in-package #:coalton-lsp)

;;; request: notebookDocument/didChange

(defun handle-notebook-document-did-change (session request)
  (declare (ignore session request))
  (error "writeme: handler notebookDocument/didChange"))

;;; request: notebookDocument/didClose

(defun handle-notebook-document-did-close (session request)
  (declare (ignore session request))
  (error "writeme: handler notebookDocument/didClose"))

;;; request: notebookDocument/didOpen

(defun handle-notebook-document-did-open (session request)
  (declare (ignore session request))
  (error "writeme: handler notebookDocument/didOpen"))

;;; request: notebookDocument/didSave

(defun handle-notebook-document-did-save (session request)
  (declare (ignore session request))
  (error "writeme: handler notebookDocument/didSave"))
