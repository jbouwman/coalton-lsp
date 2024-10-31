(defpackage #:coalton-lsp.lib.list
  (:use #:cl)
  (:export #:listify))

(in-package #:coalton-lsp.lib.list)

(defun listify (x)
  (if (listp x) x (list x)))
