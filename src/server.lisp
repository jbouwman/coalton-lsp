;;;; Functions for starting and stopping the LSP server

(in-package :coalton-lsp)

(defgeneric uri (resource))

(defgeneric input-stream (session-io))

(defgeneric output-stream (session-io))

(defgeneric stop-session (server session))

(defvar *server* nil
  "The server process.")

;;; network server

(defvar *default-port* 7887
  "The default port of LSP sessions.")

(defclass network-io ()
  ((socket :initarg :socket)))

(defmethod input-stream ((self network-io))
  (usocket:socket-stream (slot-value self 'socket)))

(defmethod output-stream ((self network-io))
  (usocket:socket-stream (slot-value self 'socket)))

(defmethod stop ((self network-io))
  (with-slots (socket) self
    (usocket:socket-close socket)))

(defclass network-server (process)
  ((session-id :initform 0
               :accessor session-id)
   (config :initarg :config)
   (listener :initform nil)
   (sessions :initform nil))
  (:documentation "A Coalton LSP server"))

(defmethod uri ((self network-server))
  "Return the string representation of SERVER's network address."
  (with-slots (config) self
    (destructuring-bind (&key host port &allow-other-keys) config
      (format nil "json-rpc://~a:~a" host port))))

(defmethod print-object ((self network-server) stream)
  (print-unreadable-object (self stream :type t :identity t)
    (write-string (uri self) stream)))

(defmethod run ((self network-server))
  (with-slots (listener sessions) self
    (handler-case
        (loop :do
          (usocket:wait-for-input listener)
          (with-lock-held (self)
            (when (and listener (usocket::state listener))
              (let ((socket (usocket:socket-accept listener
                                                   :element-type 'character)))
                (/debug "accept connection on ~a" socket)
                (create-session self socket)))))
      (usocket:bad-file-descriptor-error ()
        nil))))

(defun create-session (server socket)
  "Create a new session that communicates over SOCKET, and ad it to SERVER's session list."
  (push (start (make-instance 'session
                 :id (incf (session-id server))
                 :server server
                 :io (make-instance 'network-io
                       :socket socket)))
        (slot-value server 'sessions)))

(defmethod stop-session ((server network-server) session)
  (with-slots (lock sessions) server
    (bt:with-recursive-lock-held (lock)
      (stop session)
      (delete session sessions))))

(defmethod start ((server network-server))
  (with-slots (config listener thread) server
    (destructuring-bind (&key interface host port &allow-other-keys) config
      (setf listener (usocket:socket-listen (or interface host) port :reuse-address t))))
  (call-next-method))

(defmethod stop ((self network-server))
  (with-lock-held (self)
    (with-slots (sessions listener) self
      (when listener
        (usocket:socket-close listener)
        (setf listener nil))
      (dolist (session sessions)
        (stop session))))
  (call-next-method))

;;; local server

(defclass stream-io ()
  ((input :initarg :input
          :reader input-stream)
   (output :initarg :output
           :reader output-stream)))

(defmethod stop ((self stream-io)))

(defclass stream-server ()
  ())

(defmethod uri ((self stream-server))
  "json-rpc:stdio")

(defmethod stop-session ((self stream-server) session))

(defun start-stream-server (input output)
  (when *server*
    (error "server is running: ~a" *server*))
  (setf *server*
        (make-instance 'stream-server))
  (start (make-instance 'session
           :id 1
           :server *server*
           :io (make-instance 'stream-io
                 :input input
                 :output output))))

(defun stop-server ()
  "Close all sessions and stop the server."
  (when (null *server*)
    (/warn "coalton-lsp not running")
    (return-from stop-server))
  (stop *server*)
  (/info "~a halted" *server*)
  (setf *server* nil))

(defun start-network-server (port)
  "Run a Coalton LSP server on PORT."
  (when *server*
    (error "server is running: ~a" *server*))
  (setf *server*
        (start (make-instance 'network-server
                 :config (list :port port
                               :host "127.0.0.1"))))
  (/info "~a started" *server*))

(defun parse-argv (argv)
  (pop argv)
  (let ((port nil))
    (loop :while argv
          :do (cond ((string-equal (car argv) "--port")
                     (unless (cadr argv)
                       (error "missing PORT argument"))
                     (handler-case
                         (progn
                           (setf port (parse-integer (cadr argv)))
                           (pop argv)
                           (pop argv))
                       (error ()
                         (error "illegal PORT value ~a" (cadr argv)))))
                    (t
                     (error "unknown argument ~a" (car argv)))))
    port))

(defun main ()
  "Run the Coalton LSP server."
  (setf *logger*
        (make-instance 'stream-logger :stream *error-output* :level ':info))
  (let ((port (parse-argv sb-ext:*posix-argv*)))
    (cond (port
           (start-network-server port))
          (t
           (start-stream-server *standard-input*
                                *standard-output*)))
    (handler-case
        (loop (sleep 1))
      (sb-sys:interactive-interrupt ()
        (/info "coalton-lsp halted")
        (terpri)
        (cl-user::quit)))))
