(fiasco:define-test-package #:coalton-lsp.session.diagnostic-tests
  (:use
   #:cl
   #:coalton-lsp
   #:coalton-lsp.lib.test))

(in-package #:coalton-lsp.session.diagnostic-tests)

(defun select-case (suite n)
  (some (lambda (c)
          (when (= (second c) n)
            c))
        suite))

;; make a local session, load an example of a program containing a
;; defective toplevel form from a known location, and open it

(deftest session.diagnostic-tests/unknown-toplevel-form ()
  (let* ((session (make-instance 'coalton-lsp::session))
         (input (select-case (test-suite "diagnostic.suite") 1))
         (text (nth 4 input)))
    (coalton-lsp::open-document session "testsuite:1" text)
    ;; collect diagnostics ..
    ))
