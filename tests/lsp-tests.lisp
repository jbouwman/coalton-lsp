(in-package #:coalton-lsp.tests)

(deftest test-ini!tialize-result ()
  (let ((x (coalton-lsp.lib.message:make-message 'coalton-lsp::initialize-result)))
    (coalton-lsp.lib.message:set-field x (list :capabilities :position-encoding)
                                       :utf32)
    (set-field x (list :server-info :name)
                "Emacs")
    x))
