(in-package #:coalton-lsp.tests)

(deftest protocol-tests/initialize ()
  (let ((params (coalton-lsp::request-params
                 (coalton-lsp::make-request (rpc-example "initialize.json")))))
    (get-field params :root-uri)
    (get-field params :workspace-folders)))
