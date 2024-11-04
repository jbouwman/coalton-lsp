(in-package :coalton-lsp.tests)

(defun mock-publish-diagnostics ()
  (coalton-lsp::make-notification
   "textDocument/publishDiagnostics"
   (let ((message (new-message 'coalton-lsp::publish-diagnostics-params
                               :uri "file:///Users/jlbouwman/git/coalton-mode/resources/fib.coal")))
     (set-field message :diagnostics
                     (list (message-value
                            (coalton-lsp::make-diagnostic 4 4 4 7
                                                  "export: 'fob' is undefined"
                                                  "undefined-export"))))
     message)))
