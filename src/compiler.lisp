(in-package #:coalton-lsp)

(defclass uri-source ()
  ((uri :initarg :uri)))

(defmethod source:source-stream ((self uri-source))
  (input-stream (slot-value self 'uri)))

(defmethod source:source-available-p ((self uri-source))
  t)

(defmethod source:source-name ((self uri-source))
  (uri:uri-path (slot-value self 'uri)))

(defun parse-program (file-uri)
  (let ((uri (uri:parse file-uri)))
    (when uri
      (let* ((filename (uri:uri-path uri))
             (source (source:make-source-file filename)))
        (with-open-stream (stream (source:source-stream source))
          (parser:with-reader-context stream
            (parser:read-program stream source ':file)))))))

(defun tokenize (type locatable)
  (destructuring-bind (start . end)
      (source:location-span (source:location locatable))
    (list type start (- end start))))

(defgeneric visit-tokens (element visitor))

(defmethod visit-tokens ((self program:toplevel-define) f)
  (funcall f (tokenize :function (program:toplevel-define-name self))))

(defun visit-program (file-uri)
  (let ((program (parse-program file-uri))
        (tokens nil))
    (dolist (define (program:program-defines program))
      (visit-tokens define (lambda (x)
                             (push x tokens))))
    tokens))

;; (visit-program "file:///Users/jlbouwman/git/coalton-lsp/resources/fib.coal")

;;; 'diagnostics' is the LSP term for parser warnings and compiler errors

(defun export-condition (condition)
  "Extract text and position fields from a Coalton source condition."
  (with-open-stream (source-stream (source::condition-stream condition))
    (let ((state (source::make-printer-state source-stream condition)))
      (mapcar (lambda (note)
                (list (source:message condition)
                      (source:message note)
                      (source::offset-position state (source::start-offset note))
                      (source::offset-position state (source::end-offset note))))
              (source::notes condition)))))

(defun make-diagnostic (s1 e1 s2 e2 message code)
  (let ((diagnostic (make-message 'diagnostic)))
    (set-field diagnostic (list :range :start :line) (1- s1))
    (set-field diagnostic (list :range :start :character) e1)
    (set-field diagnostic (list :range :end :line) (1- s2))
    (set-field diagnostic (list :range :end :character) e2)
    (set-field diagnostic (list :message) message)
    (set-field diagnostic (list :code) code)
    (set-field diagnostic (list :severity) :warning)
    (set-field diagnostic (list :source) "coalton")
    diagnostic))

(defun make-diagnostics (c)
  (mapcar (lambda (e)
            (let ((coalton-impl/settings:*coalton-print-unicode* nil))
              (destructuring-bind (note message start end) e
                (message-value
                 (make-diagnostic (car start) (cdr start)
                                  (car end) (cdr end)
                                  (format nil "~a - ~a" note message)
                                  1)))))
          (export-condition c)))

(defun compile-uri (ur-uri)
  (let ((uri (uri:parse ur-uri)))
    (when uri
      (let* ((filename (uri:uri-path uri))
             (source (source:make-source-file filename)))
        (handler-case
            (progn
              (coalton-impl/entry:compile source :load nil)
              nil)
          (source::source-condition (c)
            (make-diagnostics c)))))))
