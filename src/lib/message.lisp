(defpackage #:coalton-lsp.lib.message
  (:use #:cl
        #:coalton-lsp.lib.list
        #:coalton-lsp.lib.name)
  (:export #:copy-message
           #:define-atom
           #:define-class
           #:define-enum
           #:define-union
           #:find-message-class
           #:get-field
           #:get-field-2
           #:json-key
           #:json-value
           #:make-message
           #:message
           #:message-atom
           #:message-class
           #:message-class-p
           #:message-enum
           #:message-field
           #:message-fields
           #:message-type
           #:message-union
           #:message-value
           #:new-message
           #:optional
           #:set-field
           #:set-field-2
           #:set-field-class
           #:set-field-vector-class))

(in-package #:coalton-lsp.lib.message)

(defvar *message-classes*
  (make-hash-table))

(defclass message-type ()
  ((name :initarg :name
         :reader name)))

(defclass message-atom (message-type)
  ())

(defgeneric copy-message (message)
  (:documentation "Make a shallow copy of message."))

(defgeneric json-value (message value)
  (:documentation "Use message to convert VALUE to a structure that can be directly encoded as JSON.

The output value will have string-valued map keys.")
  (:method (message value)
    (declare (ignore message))
    value))

(defclass message-field (message-type)
  ((json :initarg :json
         :reader json-key) ; field value's key in a JSON map
   (class-name :initarg :class-name
          :reader message-class-name)
   (optional :initarg :optional
             :initform nil)
   (vector :initarg :vector
           :initform nil)))

(defmethod message-class ((self message-field))
  (find-message-class (message-class-name self)))

(defmethod copy-message ((field message-field))
  (with-slots (name json class-name optional vector) field
    (make-instance 'message-field
                   :name name
                   :json json
                   :class-name class-name
                   :optional optional
                   :vector vector)))

(defmethod print-object ((self message-field) stream)
  (with-slots (name) self
    (print-unreadable-object (self stream :type t :identity t)
      (format stream "~a (~a)" name (message-class-name self)))))

(defun parse-field-type (type)
  (etypecase type
    (list
     (destructuring-bind (type &key optional vector) type
       (list type optional vector)))
    (symbol
     (list type nil nil))))

(defun make-field (spec)
  (destructuring-bind (name type) spec
    (destructuring-bind (class-name optional vector) (parse-field-type type)
      (make-instance 'message-field
                     :name name
                     :json (camel-case name)
                     :class-name class-name
                     :optional optional
                     :vector vector))))

(defclass message-class (message-type)
  ((fields :initform (make-array 0 :adjustable t :fill-pointer t)
           :reader message-fields)))

(defmethod print-object ((self message-class) stream)
  (with-slots (name fields) self
    (print-unreadable-object (self stream :type t :identity t)
      (format stream "~a (~d fields)" name (length fields)))))

(defun add-field (class field)
  (vector-push-extend field (slot-value class 'fields)))

(defmethod copy-message ((class message-class))
  (with-slots (name fields) class
    (let ((class2 (make-instance 'message-class :name name)))
      (loop :for field :across fields
            :do (add-field class2 (copy-message field)))
      class2)))

(defun set-field-class (class name field-class)
  "Replace the definition of a single foield"
  (loop :for field :across (slot-value class 'fields)
        :do (when (eq (name field) name)
              (setf (slot-value field 'class-name) (name field-class))))
  class)

(defun set-field-vector-class (class name field-class)
  "Replace the definition of a single foield"
  (loop :for field :across (slot-value class 'fields)
        :do (when (eq (name field) name)
              (setf (slot-value field 'class-name) (name field-class)
                    (slot-value field 'vector) t)))
  class)

(defun %get-field (class name)
  (loop :for field :across (slot-value class 'fields)
        :when (eq (slot-value field 'name) name)
          :do (return-from %get-field field))
  (error "undefined message field: ~a" name))

(defmacro define-atom (name)
  `(setf (gethash ',name *message-classes*)
         (make-instance 'message-atom :name ',name)))

(defmacro define-class (name parent-classes &body field-defs)
  `(let ((message (make-instance 'message-class :name ',name)))
     (loop :for class :in ',parent-classes
           :do (loop :for field :across (slot-value (find-message-class class) 'fields)
                     :do (add-field message field)))
     (loop :for field-def :in ',field-defs
           :do (add-field message (make-field field-def)))
     (setf (gethash ',name *message-classes*) message)))

(defun find-message-class (name)
  (or (gethash name *message-classes*)
      (error "undefined message class: ~a" name)))

(defun message-class-p (name)
  (not (null (gethash name *message-classes*))))

;;; Enums

(defclass message-enum (message-type)
  ((values :initform (make-hash-table))))

(defmethod print-object ((self message-enum) stream)
  (with-slots (name values) self
    (print-unreadable-object (self stream :type t :identity t)
      (format stream "~a" name))))

(defmethod json-value ((self message-enum) value)
  (with-slots (values) self
    (or (gethash value values)
        (error "undefined enum value in ~a: ~a" self value))))

(defmacro define-enum (name class-args &body value-defs)
  (declare (ignore class-args))
  `(let ((message (make-instance 'message-enum :name ',name)))
     (loop :for (k v) :in ',value-defs
           :do (setf (gethash k (slot-value message 'values)) v))
     (setf (gethash ',name *message-classes*) message)))

;;; Unions

(defclass message-union (message-type)
  ((classes :initarg :classes)))

(defmethod print-object ((self message-union) stream)
  (with-slots (name) self
    (print-unreadable-object (self stream :type t :identity t)
      (format stream "~a" name))))

(defmacro define-union (name union-classes)
  `(setf (gethash ',name *message-classes*)
         (make-instance 'message-union
                        :name ',name
                        :classes (mapcar #'find-message-class
                                         ',union-classes))))

;;; Messages compose a class and a value

(defclass message ()
  ((class :initarg :class
          :reader message-class)
   (value :initarg :value
          :initform nil
          :reader message-value)))

(defmethod print-object ((self message) stream)
  (with-slots (class value) self
    (print-unreadable-object (self stream :type t)
      (princ (name class) stream))))

(defun make-message (class &optional value)
  (assert (symbolp class))
  (make-instance 'message
                 :class (find-message-class class)
                 :value value))

(defun %get-key (message key)
  (let ((field (%get-field (message-class message) key)))
    (make-instance 'message
                   :class (message-class field)
                   :value (cdr (assoc (json-key field)
                                      (message-value message)
                                      :test #'string=)))))

(defun %get-path (message path)
  (reduce #'%get-key (listify path) :initial-value message))

(defun %set-key (message key value)
  (let ((field (%get-field (message-class message) key)))
    (make-message (name (message-class message))
                  (acons (json-key field)
                         (json-value (message-class field) value)
                         (remove (json-key field)
                                 (message-value message)
                                 :key #'car
                                 :test #'string=)))))

(defun %set-path (message path value)
  (destructuring-bind (key &rest keys) (listify path)
    (%set-key message key
              (if (null keys)
                  value
                  (message-value (%set-path (%get-key message key) keys value))))))

;; Message API

(defun get-field (message path)
  (message-value (%get-path message (listify path))))

(defun set-field-2 (message key value)
  (let ((inner (%get-path message (listify key))))
    (if (typep (message-class inner) 'message-class)
        (progn
          (loop :for (key value) :on value :by #'cddr
                :do (set-field-2 inner key value))
          (set-field message key (message-value inner)))
        (set-field message key value))
    message))

(defun set-field (message path value)
  (setf (slot-value message 'value)
        (slot-value (%set-path message (listify path) value) 'value))
  message)

(defun new-message (message-class &rest plist)
  (let ((message (make-message message-class)))
    (loop :for (key value) :on plist :by #'cddr
          :do (set-field-2 message key value))
    message))
