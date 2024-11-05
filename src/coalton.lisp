;;;; Interaction with parser, compiler and runtime

(defpackage #:coalton-lsp.coalton
  (:use #:cl)
  (:local-nicknames
   (#:parser #:coalton-impl/parser)
   (#:program #:coalton-impl/parser/toplevel)
   (#:source #:coalton-impl/source)
   (#:uri #:coalton-lsp.lib.uri))
  (:export #:document-symbols
           #:document-diagnostics
           #:export-condition
           #:program
           #:line-offsets
           #:uri-source))

(in-package #:coalton-lsp.coalton)

(defclass uri-source ()
  ((uri :initarg :uri)))

(defmethod print-object ((self uri-source) stream)
  (if *print-readably*
      (format stream "~s" (slot-value self 'uri))
      (call-next-method)))

(defun uri-source (uri)
  (make-instance 'uri-source
                 :uri (uri:parse uri)))

(defmethod source:source-stream ((self uri-source))
  (uri:input-stream (slot-value self 'uri)))

(defmethod source:source-available-p ((self uri-source))
  t)

(defmethod source:source-name ((self uri-source))
  (uri:uri-path (slot-value self 'uri)))

(defun program (source)
  "Return a program parsed from SOURCE."
  (with-open-stream (stream (source:source-stream source))
    (parser:with-reader-context stream
      (parser:read-program stream source ':file))))

(defun line-offsets (source)
  (with-open-stream (stream (source:source-stream source))
    (source::find-line-offsets stream)))

(defgeneric visit-node (visitor node)
  (:method (v d) nil))

(defun visit-nodes (visitor list)
  (dolist (node list)
    (visit-node visitor node)))

(defun visit-program (source visitor)
  (let ((ast (program source)))
    (visit-node visitor ast)
    (visit-nodes visitor (program:program-defines ast))
    (visit-nodes visitor (program:program-declares ast))))


(defclass document-symbol-collector ()
  ((symbols :initform nil
            :accessor symbols)))

(defun collect-symbol (dsc ds)
  (push ds (symbols dsc)))

(defmethod visit-node ((self document-symbol-collector) (node program:toplevel-define))
  (let ((name (program::toplevel-define-name node)))
    (collect-symbol self
                    (list :name (string-downcase
                                 (symbol-name
                                  (coalton-impl/parser/expression::node-variable-name name)))
                          ;; :detail "* -> *" run the compiler to get its signature
                          :kind :define
                          :range (source:location-span (source:location node))
                          :selection-range (source:location-span (source:location name))))))

(defmethod visit-node ((self document-symbol-collector) (node program:toplevel-declare))
  (let ((name (program::toplevel-declare-name node)))
    (collect-symbol self
                    (list :name (string-downcase
                                 (symbol-name
                                  (coalton-impl/parser/expression::identifier-src-name name)))
                          :kind :declare
                          :range (source:location-span (source:location node))
                          :selection-range (source:location-span (source:location name))))))

(defun document-symbols (source)
  (let ((visitor (make-instance 'document-symbol-collector)))
    (visit-program source visitor)
    (symbols visitor)))


;;; 'diagnostics' is the LSP term for parser warnings and compiler errors

(defun export-condition (condition)
  "Extract text and position fields from a Coalton source condition."
  (mapcar (lambda (note)
            (list (source::severity condition)
                  (source:message note)
                  (source::start-offset note)
                  (source::end-offset note)))
          (source::notes condition)))

(defun document-diagnostics (source)
  (handler-case
      (progn
        (coalton-impl/entry:codegen source)
        nil)
    (source::source-condition (c)
      (export-condition c))))
