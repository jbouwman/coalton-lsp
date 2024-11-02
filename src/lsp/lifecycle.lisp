;;;; LSP request handlers

(in-package #:coalton-lsp)

;;; request: $/logTrace

(defun handle-log-trace (session request)
  (declare (ignore session request))
  (error "writeme: handler $/logTrace"))

;;; request: $/setTrace

(defun handle-set-trace (session request)
  (declare (ignore session request))
  (error "writeme: handler $/setTrace"))

;;; request: client/registerCapability

(defun handle-client-register-capability (session request)
  (declare (ignore session request))
  (error "writeme: handler client/registerCapability"))

;;; request: client/unregisterCapability

(defun handle-client-unregister-capability (session request)
  (declare (ignore session request))
  (error "writeme: handler client/unregisterCapability"))

;;; request: exit

(defun handle-exit (session request)
  (declare (ignore session request))
  (error "writeme: handler exit"))

;;; request: initialize

(defun handle-initialize (session request)
  (initializing-session session (message-value (request-params request)))
  (make-response
   (get-field request :id)
   (new-message
    'initialize-result
    :server-info '(:name "Coalton")
    :capabilities `(:text-document-sync (:open-close t
                                         :change :full)
                    :definition-provider (:work-done-progress t)
                    :document-formatting-provider (:work-done-progress t)
                    :document-symbol-provider t
                    :semantic-tokens-provider (:legend (:token-types
                                                        ("namespace" "type"
                                                                     "function" "macro"
                                                                     "keyword" "class"
                                                                     "variable" "method"
                                                                     "event" "interface")
                                                        :token-modifiers
                                                        ("definition"
                                                         "defaultLibrary"
                                                         "implementation"))
                                               :range t
                                               :full t)
                    :position-encoding ,(position-encoding session)))))

;;; request: initialized

(defun handle-initialized (session request)
  (declare (ignore request))
  (initialized-session session))

;;; request: shutdown

(defun handle-shutdown (session request)
  (shutdown-session session)
  (make-response (get-field request :id)
                 (make-message 'empty)))

;;; request: telemetry/event

(defun handle-telemetry-event (session request)
  (declare (ignore session request))
  (error "writeme: handler telemetry/event"))
