(fiasco:define-test-package #:coalton-lsp.tests
  (:use
   #:cl
   #:coalton-lsp
   #:coalton-lsp.lib.process
   #:coalton-lsp.lib.name
   #:coalton-lsp.lib.message
   #:coalton-lsp.lib.json
   #:coalton-lsp.lib.json-rpc
   #:coalton-lsp.lib.test))             ; assertion helpers: defined by ./test.lisp
