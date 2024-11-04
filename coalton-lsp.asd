(in-package :asdf-user)

(defsystem #:coalton-lsp
  :depends-on (#:alexandria
               #:bordeaux-threads
               #:coalton
               #:com.inuoe.jzon
               #:usocket)
  :pathname "src"
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
               (:file "coalton")
               (:file "package")
               (:module "session"
                :serial t
                :components ((:file "session")
                             (:file "diagnostic")
                             (:file "document-symbol")))
               (:module "lsp"
                :serial t
                :components ((:file "lsp")
                             (:file "document")
                             (:file "navigation")
                             (:file "lifecycle")
                             (:file "notebook")
                             (:file "window")
                             (:file "workspace")))
               (:file "server")))

(defsystem #:coalton-lsp/tests
  :depends-on (#:coalton-lsp
               #:fiasco)
  :pathname "tests"
  :serial t
  :components ((:file "laoder")
               (:file "test")
               (:file "package")
               (:file "mock")
               (:module "lib"
                :serial t
                :components ((:file "json-tests")
                             (:file "json-rpc-tests")
                             (:file "message-tests")))
               (:file "lsp-tests")
               (:file "coalton-tests")
               (:file "protocol-tests")
               (:module "session"
                :serial t
                :components ((:file "session-tests")))))
