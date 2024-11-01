(in-package :coalton-lsp.tests)

(defun mock-publish-diagnostics ()
  (coalton-lsp::make-notification
   "textDocument/publishDiagnostics"
   (let ((message (make-message 'coalton-lsp::publish-diagnostics-params)))
     (set-field message :uri "file:///Users/jlbouwman/git/coalton-mode/resources/fib.coal")
     (set-field message :diagnostics
                     (list (message-value
                            (coalton-lsp::make-diagnostic 4 4 4 7
                                                  "export: 'fob' is undefined"
                                                  "undefined-export"))))
     message)))
