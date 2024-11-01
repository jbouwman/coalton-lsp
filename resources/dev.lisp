;;;; helpers for interactive development: starting network server,
;;;; changing logging options

(defpackage #:coalton-lsp.dev
  (:use #:cl
        #:coalton-lsp)
  (:shadow #:restart))

(in-package #:coalton-lsp-user)

(defun restart ()
  (when *server*
    (stop-server))
  (start-network-server *server-port*))

;;; (restart)

(defun enable-debugging ()
  (coalton-lsp.lib.log:set-log-level :debug))

(defun enable-worker-debugging ()
  (setf coalton-lsp.lib.process::*worker-debug* t))

(defun enable-file-logger ()
  (coalton-lsp.lib.log::set-log-file "~/git/coalton-mode/rpc.log"))
