(in-package #:coalton-lsp.tests)

(deftest rpc-tests/headers ()
  (let ((parsed-headers
          '(("Content-Length" . "2")
            ("Content-Type" . "application/json")))
        (serialized-headers
          "Content-Length: 2
Content-Type: application/json

"))
    (is-string= (with-output-to-string (stream)
                  (write-headers stream parsed-headers))
                serialized-headers)
    (is (equalp (with-input-from-string (stream serialized-headers)
                  (read-headers stream))
                parsed-headers))))
