(defpackage #:coalton-lsp
  (:documentation "An LSP server for the Coalton language")
  (:use #:cl
        #:coalton-lsp.lib.json
        #:coalton-lsp.lib.json-rpc
        #:coalton-lsp.lib.list
        #:coalton-lsp.lib.log
        #:coalton-lsp.lib.name
        #:coalton-lsp.lib.process
        #:coalton-lsp.lib.message)
  (:local-nicknames
   (#:parser #:coalton-impl/parser)
   (#:program #:coalton-impl/parser/toplevel)
   (#:source #:coalton-impl/source)
   (#:uri #:coalton-lsp.lib.uri))
  (:export #:main
           #:*server*
           #:*server-port*
           #:start-network-server
           #:start-server
           #:stop-server))
