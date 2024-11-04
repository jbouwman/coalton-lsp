;;;; LSP request handlers

(in-package #:coalton-lsp)

;;; request: callHierarchy/incomingCalls

(defun handle-call-hierarchy-incoming-calls (session request)
  (declare (ignore session request))
  (error "writeme: handler callHierarchy/incomingCalls"))

;;; request: callHierarchy/outgoingCalls

(defun handle-call-hierarchy-outgoing-calls (session request)
  (declare (ignore session request))
  (error "writeme: handler callHierarchy/outgoingCalls"))

;;; request: codeAction/resolve

(defun handle-code-action-resolve (session request)
  (declare (ignore session request))
  (error "writeme: handler codeAction/resolve"))

;;; request: codeLens/resolve

(defun handle-code-lens-resolve (session request)
  (declare (ignore session request))
  (error "writeme: handler codeLens/resolve"))

;;; request: completionItem/resolve

(defun handle-completion-item-resolve (session request)
  (declare (ignore session request))
  (error "writeme: handler completionItem/resolve"))

;;; request: documentLink/resolve

(defun handle-document-link-resolve (session request)
  (declare (ignore session request))
  (error "writeme: handler documentLink/resolve"))

;;; request: inlayHint/resolve

(defun handle-inlay-hint-resolve (session request)
  (declare (ignore session request))
  (error "writeme: handler inlayHint/resolve"))

;;; request: typeHierarchy/subtypes

(defun handle-type-hierarchy-subtypes (session request)
  (declare (ignore session request))
  (error "writeme: handler typeHierarchy/subtypes"))

;;; request: typeHierarchy/supertypes

(defun handle-type-hierarchy-supertypes (session request)
  (declare (ignore session request))
  (error "writeme: handler typeHierarchy/supertypes"))
