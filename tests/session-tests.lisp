(in-package #:coalton-lsp.tests)

(deftest session-tests/initialize ()
  (let ((session (make-instance 'coalton-lsp::session)))
    (is (equalp (message-value
                 (coalton-lsp::process-request
                  session (coalton-lsp::make-request (rpc-example "initialize.json"))))
                '(("result"
                   ("capabilities"
                    ("positionEncoding"
                     . "utf-16")
                    ("semanticTokensProvider"
                     ("full" . T)
                     ("range" . T)
                     ("legend"
                      ("tokenModifiers"
                       "definition"
                       "defaultLibrary"
                       "implementation")
                      ("tokenTypes"
                       "namespace"
                       "type"
                       "function"
                       "macro"
                       "keyword"
                       "class"
                       "variable"
                       "method"
                       "event"
                       "interface")))
                    ("documentSymbolProvider"
                     . T)
                    ("documentFormattingProvider"
                     ("workDoneProgress"
                      . T))
                    ("definitionProvider"
                     ("workDoneProgress"
                      . T))
                    ("textDocumentSync"
                     ("change" . 1)
                     ("openClose"
                      . T)))
                   ("serverInfo" ("name" . "Coalton")))
                  ("id" . 1) ("jsonrpc" . "2.0"))))))

(deftest session-tests/get-field ()
  (let ((init (coalton-lsp::make-request (rpc-example "initialize.json"))))
    (is (eq 1 (get-field init :id)))

    (let ((params (coalton-lsp::request-params init)))
      (is (eq t
              (get-field params '(:capabilities :workspace
                                  :did-change-watched-files :dynamic-registration)))))))

(deftest session-tests/set-field ()
  (let ((params (new-message 'coalton-lsp::initialize-params)))
    (message-value (coalton-lsp.lib.message::%set-key params :capabilities 'x)))
  (new-message 'coalton-lsp::initialize-params
               :capabilities '(:workspace x)))

(deftest session-tests/encode-json ()
  (is-string= (coalton-lsp::to-json
               (let ((session (make-instance 'coalton-lsp::session)))
                 (coalton-lsp::process-request
                  session (coalton-lsp::make-request (rpc-example "initialize.json")))))
              "{
  \"jsonrpc\": \"2.0\",
  \"id\": 1,
  \"result\": {
    \"capabilities\": {
      \"positionEncoding\": \"utf-16\",
      \"textDocumentSync\": {
        \"openClose\": true,
        \"change\": 1
      },
      \"definitionProvider\": {
        \"workDoneProgress\": true
      },
      \"documentSymbolProvider\": true,
      \"documentFormattingProvider\": {
        \"workDoneProgress\": true
      },
      \"semanticTokensProvider\": {
        \"legend\": {
          \"tokenTypes\": [
            \"namespace\",
            \"type\",
            \"function\",
            \"macro\",
            \"keyword\",
            \"class\",
            \"variable\",
            \"method\",
            \"event\",
            \"interface\"
          ],
          \"tokenModifiers\": [
            \"definition\",
            \"defaultLibrary\",
            \"implementation\"
          ]
        },
        \"range\": true,
        \"full\": true
      }
    },
    \"serverInfo\": {
      \"name\": \"Coalton\"
    }
  }
}"))
