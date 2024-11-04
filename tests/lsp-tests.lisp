(in-package #:coalton-lsp.tests)

(deftest test-initialize-result ()
  (let ((x (new-message 'coalton-lsp::initialize-result
                        :capabilities '(:position-encoding :utf32)
                        :server-info '(:name "Emacs"))))
    x))
