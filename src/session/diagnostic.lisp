(in-package #:coalton-lsp)

(defun diagnostics (source)
  (let ((diagnostics (coalton:document-diagnostics source)))
    (when diagnostics
      (let ((offsets (coalton:line-offsets source)))
        (mapcar (lambda (diagnostic)
                  (destructuring-bind (severity message start end) diagnostic
                    (list :message message
                          :severity severity
                          :source "coalton"
                          :range (offsets->positions offsets start end))))
                diagnostics)))))

(defun update-diagnostics (session uri)
  (let* ((source (document-source session uri))
         (diagnostics (diagnostics source)))
    ;; TODO message parameter type can be inferred from the method, above
    (notify session "textDocument/publishDiagnostics"
            (new-message 'publish-diagnostics-params
                         :uri uri
                         :diagnostics diagnostics))))
