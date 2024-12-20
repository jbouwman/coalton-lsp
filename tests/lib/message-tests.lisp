(fiasco:define-test-package #:coalton-lsp.lib.message.tests
  (:use
   #:cl
   #:coalton-lsp.lib.message))

(in-package #:coalton-lsp.lib.message.tests)

(define-class test-message ()
  (:enable boolean)
  (:flags (string :vector t))
  (:debug (boolean :optional t)))

(deftest message-tests/value ()
  (let ((message (new-message 'test-message
                              :debug t
                              :flags '("a" "b" "g"))))
    (is (equalp (message-value message)
                '(("flags" "a" "b" "g")
                  ("debug" . T))))
    message))
