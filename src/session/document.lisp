;;;; Track which documents are open in the workspace. Documents are
;;;; keyed by uri.

(in-package #:coalton-lsp)

;; key open documents by uri

(defun open-document (session uri document-text)
  (with-session-context (session)
    (/info "open ~a" uri)
    (cond ((gethash uri (session-documents session))
           (/warn "already open ~a" uri))
          (t
           (setf (gethash uri (session-documents session)) document-text)
           (submit-event session 'document-opened uri)))))

(defun change-document (session document)
  (with-session-context (session)
    (let ((uri (cdr (assoc "uri" document :test #'string=))))
      (submit-event session 'document-changed uri))))

(defun document-opened (session uri)
  (update-diagnostics session uri))

(defun document-changed (session uri)
  (update-diagnostics session uri))

;;; TODO when documents are opened source test is passed to the
;;; server. We can, if we want, work in-memory. Does the session want
;;; to mediate all uri access? Probably.

(defun uri-key (uri)
  (uri:as-string (uri:parse uri)))

(defun document-source (session uri)
  (let* ((documents (session-documents session))
         (uri-key (uri-key uri))
         (document (gethash uri-key documents)))
    (unless document
      (error "Unknown document ~a" uri))
    (coalton-impl/source:make-source-string document)))

