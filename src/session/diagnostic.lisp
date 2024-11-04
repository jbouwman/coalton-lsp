(in-package #:coalton-lsp)

(defun make-diagnostic (s1 e1 s2 e2 message code)
  (new-message 'diagnostic
               :message message
               :code code
               :severity :warning
               :source "coalton"
               :range `(:start (:line ,(1- s1)
                                :character ,e1)
                        :end (:line ,(1- s2)
                              :character ,e2))))

(defun diagnostics (source)
  (coalton:document-diagnostics source))

(defun update-diagnostics (session uri)
  (let* ((source (document-source session uri))
         (diagnostics (diagnostics source)))
    (notify session "textDocument/publishDiagnostics"
            (new-message 'publish-diagnostics-params ; TODO simplify: message parameter type can be inferred from the method, above
                         :uri uri
                         :diagnostics diagnostics))))
