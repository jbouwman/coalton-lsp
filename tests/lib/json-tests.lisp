(fiasco:define-test-package #:coalton-lsp.lib.json-tests
  (:use
   #:cl
   #:coalton-lsp.lib.json))

(in-package #:coalton-lsp.lib.json-tests)

(deftest test-decode-json ()
  (is (equalp (decode-json
               "{\"key\": \"value\"}")
              '(("key" . "value")))))
