;;;; LSP request handlers

(in-package #:coalton-lsp)

;;; request: window/logMessage

(defun handle-window-log-message (session request)
  (declare (ignore session request))
  (error "writeme: handler window/logMessage"))

;;; request: window/showDocument

(defun handle-window-show-document (session request)
  (declare (ignore session request))
  (error "writeme: handler window/showDocument"))

;;; request: window/showMessage

(defun handle-window-show-message (session request)
  (declare (ignore session request))
  (error "writeme: handler window/showMessage"))

;;; request: window/showMessageRequest

(defun handle-window-show-message-request (session request)
  (declare (ignore session request))
  (error "writeme: handler window/showMessageRequest"))

;;; request: window/workDoneProgress/cancel

(defun handle-window-work-done-progress-cancel (session request)
  (declare (ignore session request))
  (error "writeme: handler window/workDoneProgress/cancel"))

;;; request: window/workDoneProgress/create

(defun handle-window-work-done-progress-create (session request)
  (declare (ignore session request))
  (error "writeme: handler window/workDoneProgress/create"))
