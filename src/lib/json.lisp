(defpackage #:coalton-lsp.lib.json
  (:use #:cl
        #:coalton-lsp.lib.message)
  (:export #:decode-json
           #:reencode-json
           #:to-json
           #:to-json-array))

(in-package #:coalton-lsp.lib.json)

;;; Decoding JSON strings

;;; Jzon represents JSON maps as hashtables: import them to alists, so
;;; that messages can use a functional update model.

(defun decode-json-object (object)
  (typecase object
    (string object)
    (hash-table
     (loop :for key :being :the :hash-keys :of object
           :for value :being :the :hash-values :of object
           :collect (cons key (decode-json-object value))))
    (vector
     (loop :for element :across object
           :collect (decode-json-object element)))
    (t object)))

(defun decode-json (json)
  (decode-json-object (com.inuoe.jzon:parse json)))

;;; Encoding

(defun encode-json (value)
  (with-output-to-string (stream)
    (com.inuoe.jzon:with-writer (writer :stream stream :pretty t)
      (com.inuoe.jzon:write-value writer value))))

(defun reencode-json (string)
  (with-output-to-string (stream)
    (com.inuoe.jzon:with-writer (writer :stream stream :pretty t)
      (com.inuoe.jzon:write-value writer (com.inuoe.jzon:parse string)))))

(defgeneric jzon-value (message-class value)
  (:documentation "Convert a message to a value that can be directly serialized by jzon.")
  (:method (message-class value)
    value))

;;; The values of atoms are all lisp atomic types that jzon serializes
;;; correctly.

(defmethod jzon-value ((self message-atom) value)
  (cond ((atom value)
         (or value 'null))
        (t
         (error "non-atom value in atom field: check the field type? value: ~s" value))))

;;; Construct a jzon value, considering the 'vector and 'optional
;;; properties of a field.

(defun jzon-field-value (field value)
  (with-slots (class-name vector) field
    (cond (vector
           (loop :with result := (make-array (length value))
                 :for element :in value
                 :for i :below (length value)
                 :do (setf (aref result i)
                           (jzon-value (find-message-class class-name) element))
                 :finally (return result)))
          (t
           (jzon-value (find-message-class class-name) value)))))

(defmethod jzon-value ((self message-class) value)
  (let ((jzon-map (make-hash-table :test 'equal)))
    (loop :for field :across (message-fields self)
          :do (with-slots (resolve optional) field
                (let ((field-value (cdr (assoc (json-key field) value :test #'string=))))
                  (when (or field-value (not optional))
                    (setf (gethash (json-key field) jzon-map)
                          (jzon-field-value field field-value))))))
    jzon-map))

(defun to-json (message)
  (encode-json (jzon-value (message-class message)
                           (message-value message))))

(defun to-json-array (array)
  (let ((jzon-value (make-array (length array))))
    (loop :for i :below (length array)
          :do (setf (aref jzon-value i)
                    (jzon-value (message-class (nth i array))
                                (message-value (nth i array)))))
    (encode-json jzon-value)))
