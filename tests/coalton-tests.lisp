(fiasco:define-test-package #:coalton-lsp.coalton-tests
  (:use
   #:cl
   #:coalton-lsp.coalton))

(in-package #:coalton-lsp.coalton-tests)

(defun system-url (system path)
  (let ((base-path (directory-namestring (asdf:system-source-directory system))))
    (coalton-lsp.lib.uri:parse (format nil "file://~a~a" base-path path))))

(defvar *fib*
  (system-url "coalton-lsp" "resources/fib.coal"))

(deftest test-program ()
  (is (program (uri-source *fib*))))

(deftest test-line-offsets ()
  (is (line-offsets (uri-source *fib*))))

(deftest test-document-symbols ()
  (is (document-symbols (uri-source *fib*))))

(deftest test-document-diagnostics ()
  (document-diagnostics (uri-source *fib*))
  (is t))

