(in-package :coalton-lsp)

(defclass session (process)
  ((id :initarg :id
       :reader session-id)
   (server :initarg :server)
   (io :initarg :io)
   (worker :initform (make-instance 'worker)
                :reader worker)
   (state :accessor session-state
          :initform 'uninitialized)
   (params :initform nil
           :accessor session-params)
   (documents :initform (make-hash-table :test #'equal)
              :accessor session-documents))
  (:documentation "Per-connection session data & runloop."))

(defmacro with-session-context ((session) &body body)
  `(with-logging-context (:session (lambda (stream)
                                     (format stream "session ~d" (session-id ,session))))
     ,@body))

(defun submit-event (session method value)
  (with-session-context (session)
    (/trace "submit-event ~a ~a" method value)
    (with-slots (worker) session
      (enqueue worker (cons method value)))))

(defun process-event (session event)
  (with-session-context (session)
    (destructuring-bind (method . value) event
      (/trace "process-event ~a" method)
      (funcall method session value))))

(defun client-root-uri (session)
  (cdr (assoc "root-uri" (session-params session) :test #'string=)))

(defmethod uri ((self session))
  (with-slots (server id) self
    (format nil "~a/~a" (uri server) id)))

(defmethod print-object ((self session) stream)
  (print-unreadable-object (self stream :type t)
    (format stream "~a root ~a"
            (uri self)
            (client-root-uri self))))

(defun initializing-session (session params)
  (setf (session-state session) 'initializing)
  (setf (session-params session) params))

(defun shutdown-session (session)
  (setf (session-state session) 'shutting-down))

(defun initialized-session (session)
  (with-session-context (session)
    (setf (session-state session) 'initialized)
    (/info "initialized")
    nil))

(defun position-encoding (session)
  (declare (ignore session))
  ':utf16)                              ; TODO consult capabilities

(defun update-configuration (session config)
  (with-session-context (session)
    (/info "updated configuration: ~a" config)
    nil))

;; key open documents by uri

(defun open-document (session document)
  (with-session-context (session)
    (let ((uri (cdr (assoc "uri" document :test #'string=))))
      (/info "open ~a" uri)
      (cond ((gethash uri (session-documents session))
             (/warn "already open ~a" uri))
            (t
             (setf (gethash uri (session-documents session)) document)
             (submit-event session 'document-opened uri))))))

(defun change-document (session document)
  (with-session-context (session)
    (let ((uri (cdr (assoc "uri" document :test #'string=))))
      (submit-event session 'document-changed uri))))

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

(defun update-diagnostics (session uri)
  (notify session "textDocument/publishDiagnostics" 
          (new-message 'publish-diagnostics-params
                       :uri uri
                       :diagnostics (compile-uri uri))))

(defun document-opened (session uri)
  (update-diagnostics session uri))

(defun document-changed (session uri)
  (update-diagnostics session uri))

(define-condition session-exit ()
  ())

(defun message-type (rpc-message)
  (if (rpc-message-field rpc-message :id)
      'request-message
      'notification-message))

(defun make-request (rpc-message)
  (make-message (message-type rpc-message)
                (parsed-content rpc-message)))

;;; The message class of the 'result' field in a response message is
;;; that of the result message. Build a customized response-message by
;;; using that to set the field type so that the serializer can do its
;;; work. The only other thing needed is the associated request id.

(defun make-response (id result)
  (let ((class (copy-message (find-message-class 'response-message))))
    (set-field-class class :result (message-class result))
    (let ((response (make-instance 'message :class class)))
      (set-field response :jsonrpc "2.0")
      (set-field response :id id)
      (set-field response :result (message-value result))
      response)))

(defun make-vector-response (id result)
  (let ((class (copy-message (find-message-class 'response-message))))
    (set-field-vector-class class :result (message-class (first result)))
    (let ((response (make-instance 'message :class class)))
      (set-field response :jsonrpc "2.0")
      (set-field response :id id)
      (set-field response :result (mapcar #'message-value result))
      response)))

(defun make-error-response (id condition)
  (let ((response (new-message 'response-message
                               :jsonrpc "2.0"
                               :id id)))
    (set-field response (list :error :code) (error-code condition))
    (set-field response (list :error :message) (error-message condition))
    response))

(defun make-notification (method params)
  (let ((class (copy-message (find-message-class 'notification-message))))
    (set-field-class class :params (message-class params))
    (let ((notification (make-instance 'message :class class)))
      (set-field notification :jsonrpc "2.0")
      (set-field notification :method method)
      (set-field notification :params (message-value params))
      notification)))

(defun notify (session method value)
  (submit-event session 'write-message (make-notification method value)))

(define-condition lsp-error (error)
  ((code :initarg :code
         :accessor error-code)
   (message :initarg :message
            :accessor error-message)))

(defun response-error (error-code args)
  (apply #'/error args)
  (error 'lsp-error
         :code error-code
         :message (apply #'format nil args)))

(defun invalid-request (&rest args)
  (response-error :invalid-request args))

(defun method-not-found (&rest args)
  (response-error :method-not-found args))

(defun request-method (request)
  (let ((rpc-version (get-field request :jsonrpc))
        (method (get-field request :method)))
    (cond ((not (string-equal rpc-version "2.0"))
           (invalid-request "bad rpc version: ~a" rpc-version))
          ((not method)
           (invalid-request "missing method")))
    method))

(defvar *requests*
  (make-hash-table :test 'equal))

(defstruct request-handler
  params
  function)

(defun get-request-handler (method)
  (let ((handler (gethash method *requests*)))
    (unless handler
      (method-not-found "unsupported method: ~a" method))
    handler))

(defun %define-request (&key method params function)
  (unless (fboundp function)
    (warn "request handler function is undefined: ~a" function))
  (unless (message-class-p params)
    (warn "request parameter class is undefined: ~a" params))
  (setf (gethash method *requests*)
        (make-request-handler :params params
                              :function function)))

(defmacro define-request (method params function)
  `(%define-request :method ,method
                    :params ',params
                    :function ',function))

(defun request-params (request)
  "Return REQUEST's params message."
  (let* ((method (request-method request))
         (params-class (request-handler-params (gethash method *requests*))))
    (when params-class
      (make-message params-class (get-field request :params)))))

(defun process-notification (session request)
  (handler-case
      (let ((handler (get-request-handler (request-method request))))
        (funcall (request-handler-function handler) session request))
    (lsp-error ()
      ;; A message was logged when the error was signaled, and
      ;; there's nothing to return to the client. No-op.
      )))

(defun process-request (session request)
  (handler-case
      (let ((handler (get-request-handler (request-method request))))
        (funcall (request-handler-function handler) session request))
    (lsp-error (condition)
      (make-error-response (get-field request :id) condition))))

(defun process-message (session message)
  (let ((request (make-request message)))
    (cond ((rpc-message-field message :id)
           (submit-event session 'write-message
                         (process-request session request)))
          (t
           (process-notification session request)))))

(defun write-message (session response)
  (let ((json (to-json response)))
    (with-lock-held (session)
      (with-slots (io) session
        (write-rpc json (output-stream io))))))

(defmethod run ((self session))
  (with-slots (io worker server) self
    (setf (worker-function worker)
          (lambda (event)
            (process-event self event)))
    (start worker)
    (handler-case
        (loop :do
          (handler-case
              (progn
                (let ((message (read-rpc (input-stream io))))
                  (submit-event self 'process-message message)))
            (sb-int:closed-stream-error ()
              (/info "session disconnected: stream closed")
              (signal 'session-exit))
            (end-of-file ()
              (/info "session disconnected: end of file")
              (signal 'session-exit))
            (error (c)
              (/error "aborted read: session shutdown: ~a" c)
              (signal 'session-exit))))
      (session-exit ()
        (stop-session server self)))))

(defmethod stop ((self session))
  (with-session-context (self)
    (/info "stopping")
    (with-slots (io worker) self
      (stop worker)
      (stop io))
    (call-next-method)))
