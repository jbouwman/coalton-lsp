(in-package :asdf-user)

(defsystem #:coalton-lsp
  :depends-on (#:alexandria
               #:bordeaux-threads
               #:coalton
               #:com.inuoe.jzon
               #:usocket)
  :pathname "src/"
  :serial t
  :components ((:module "lib"
                :serial t
                :components ((:file "log")
                             (:file "list")
                             (:file "name")
                             (:file "process")
                             (:file "message")
                             (:file "json")
                             (:file "json-rpc")
                             (:file "uri")))
               (:file "package")
               (:file "compiler")
               (:file "session")
               (:file "request-value")
               (:file "request-handler")
               (:file "server")))

(defsystem #:coalton-lsp/tests
  :depends-on (#:coalton-lsp
               #:fiasco)
  :pathname "tests/"
  :serial t
  :components ((:file "package")
               (:file "mock")
               (:module "lib"
                :serial t
                :components ((:file "json-tests")
                             (:file "message-tests")))
               (:file "lsp-tests")
               (:file "protocol-tests")
               (:file "rpc-tests")
               (:file "session-tests")))
