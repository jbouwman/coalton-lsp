(fiasco:define-test-package #:coalton-lsp.session.diagnostic-tests
  (:use
   #:cl
   #:coalton-lsp
   #:coalton-lsp.lib.test))

(in-package #:coalton-lsp.session.diagnostic-tests)

;; make a local session, load an example of a program containing a
;; defective toplevel form from a known location, and open it

(deftest session.diagnostic-tests/unknown-toplevel-form ()
  (let* ((session (make-instance 'coalton-lsp::session))
         (input (test-case "diagnostic.suite" 1))
         (text (nth 4 input)))
    (coalton-lsp::open-document session "file:///unknown-1" text)
    (coalton-lsp::new-message
     'coalton-lsp::publish-diagnostics-params
     :diagnostics
     (coalton-lsp::diagnostics (coalton-lsp::document-source session "file:///unknown-1")))))
