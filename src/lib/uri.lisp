;;;; Just enough to parse simple file: scheme URLs

(defpackage #:coalton-lsp.lib.uri
  (:use #:cl)
  (:export #:parse
           #:as-string
           #:uri
           #:uri-path
           #:input-stream))

(in-package #:coalton-lsp.lib.uri)

(defstruct uri
  scheme
  path)

(defun parse (uri)
  (etypecase uri
    (uri uri)
    (string (cond ((alexandria:starts-with-subseq "file:" uri)
                   (make-uri :scheme "file"
                             :path (subseq uri 7)))
                  (t
                   (error "unsupported uri protocol: ~a" uri)))))) ; FIXME split at ':'

(defun as-string (uri)
  (format nil "~a://~a"
          (uri-scheme uri)
          (uri-path uri)))

(defvar *stream-providers*
  (make-hash-table :test #'equalp))

(defstruct stream-provider
  input
  output)

(defun define-stream-provider (name ignore in out)
  (declare (ignore ignore))
  (setf (gethash name *stream-providers*)
        (make-stream-provider :input in
                              :output out)))

(define-stream-provider "file" ()
  (lambda (uri)
    (open (uri-path uri)
          :direction :input
          :element-type 'character))
  (lambda (uri)
    (open (uri-path uri)
          :direction :output
          :element-type 'character)))

(defun stream-provider (url)
  (or (gethash (uri-scheme url) *stream-providers*)
      (error "No stream provider for scheme ~A" (uri-scheme url))))

(defun input-stream (url)
  (funcall (stream-provider-input (stream-provider url)) url))
