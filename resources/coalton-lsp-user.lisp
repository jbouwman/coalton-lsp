;;;; helpers for interactive development: starting network server,
;;;; changing logging options

(defpackage #:coalton-lsp-user
  (:use #:cl
        #:coalton-lsp))

(in-package #:coalton-lsp-user)

(defun restart-server ()
  (when *server*
    (stop-server))
  (coalton-lsp::start-network-server coalton-lsp::*default-port*))

;;; (restart-server)

(defun enable-debugging ()
  (coalton-lsp.lib.log:set-log-level :debug))

(defun enable-worker-debugging ()
  (setf coalton-lsp.lib.process::*worker-debug* t))

(defun enable-file-logger ()
  (coalton-lsp.lib.log::set-log-file "~/git/coalton-mode/rpc.log"))
