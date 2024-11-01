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
          (values (parser:with-reader-context stream
                    (parser:read-program stream source ':file))
                  (source::find-line-offsets stream)))))))

(defun probe-offset (offsets index value)
  (let ((lo (nth index offsets))
        (hi (nth (1+ index) offsets)))
    (cond ((< value lo) -1)
          ((not hi)      0)
          ((< hi value)  1)
          (t             0))))

(defun find-line (offsets value)
  (loop :with low := 0
        :with high := (length offsets)
        :with index := (floor (/ (+ low high) 2))
        :do (case (probe-offset offsets index value)
              ( 0 (return (values index (nth index offsets))))
              (-1 (setf high index))
              ( 1 (setf low index)))
            (setf index (floor (/ (+ low high) 2)))))

(defun offset-position (offsets location)
  (destructuring-bind (start . end)
      (source:location-span location)
    (multiple-value-bind (start-line start-offset)
        (find-line offsets start)
      (multiple-value-bind (end-line end-offset)
          (find-line offsets end)
        `(("start" .
                   (("line" . ,start-line)
                    ("character" . ,(- start start-offset))))
          ("end" .
                 (("line" . ,end-line)
                  ("character" . ,(- end end-offset)))))))))

(defgeneric lsp-visit-define (visitor offsets define))

(defclass document-symbol-visitor ()
  ((symbols :initform nil
            :accessor symbols)))

(defun %mds (name detail kind range selection-range)
  (let ((message (make-message 'document-symbol)))
    (set-field message :name name)
    (set-field message :detail detail)
    (set-field message :kind kind)
    (set-field message :range range)
    (set-field message :selection-range selection-range)
    message))

(defmethod lsp-visit-define ((self document-symbol-visitor) offsets toplevel-define)
  (let ((name (program::toplevel-define-name toplevel-define)))
    (push (%mds (string-downcase
                 (symbol-name
                  (coalton-impl/parser/expression::node-variable-name name)))
                "Integer -> Integer"
                :function
                (offset-position offsets (source:location toplevel-define))
                (offset-position offsets (source:location name)))
          (symbols self))))

(defmethod visit-program-node ((self program:toplevel-define) visitor)
  (funcall visitor self))

(defun visit-program (visitor offsets program)
  (dolist (define (program:program-defines program))
    (lsp-visit-define visitor offsets define)))

(defun document-symbols (file-uri)
  (multiple-value-bind (program offsets)
      (parse-program file-uri)
    (let ((visitor (make-instance 'document-symbol-visitor)))
      (visit-program visitor offsets program)
      (symbols visitor))))

;; (document-symbols "file:///Users/jlbouwman/git/coalton-lsp/resources/fib.coal")

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
