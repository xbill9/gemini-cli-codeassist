(in-package :mcp-server)

;;; Monkey Patches for 40ants-mcp and openrpc-server to ensure MCP compliance

(in-package :40ants-mcp/server/api/tools/list)

(defun make-tool-description-for-method (method-name method-info)
  (multiple-value-bind (params required)
      (get-method-params method-info)
    (make-instance
     'tool-description
     :name method-name
     :description (openrpc-server/method::method-summary method-info)
     :input-schema (make-instance
                    'input-schema
                    :properties params
                    :required (or required #())))))


(in-package :openrpc-server/interface)

(defmethod transform-result :around ((object standard-object))
  (let ((result (call-next-method)))
    (when (hash-table-p result)
      (multiple-value-bind (val present-p)
          (gethash "is_error" result)
        (when present-p
          (remhash "is_error" result)
          (setf (gethash "isError" result) val)))
      (multiple-value-bind (val present-p)
          (gethash "input_schema" result)
        (when present-p
          (remhash "input_schema" result)
          (setf (gethash "inputSchema" result) val))))
    result))


(in-package :40ants-mcp/server/api/tools/call)

(openrpc-server:define-rpc-method (mcp-server tools/call) (name arguments)
  (:summary "A method for calling an server tool with given NAME")
  (:description "Called when MCP client wants do something using a tool.")
  (:param name string "A tool name")
  (:param arguments hash-table  "Arguments of a tool.")
  (:result tool-call-response)
  (log:info "Called tool" name)
  
  (let ((tool (search-tool name (40ants-mcp/server/definition:server-tools-collections mcp-server))))
    (cond
      (tool
       (handler-case
           (let* ((thunk (openrpc-server/method::method-thunk tool))
                  (result (funcall thunk arguments)))
             (make-instance 'tool-call-response
                            :is-error yason:false
                            :content (uiop:ensure-list result)))
         (40ants-mcp/server/errors:tool-error (condition)
           (make-instance 'tool-call-response
                          :is-error t
                          :content (uiop:ensure-list
                                    (40ants-mcp/server/errors:tool-error-content condition))))))
      (t
       (make-instance 'tool-call-response
                      :is-error t
                      :content (list
                                (make-instance '40ants-mcp/content/text:text-content
                                               :text (serapeum:fmt "Tool \"~A\" does not exist."
                                                                   name))))))))


(in-package :40ants-mcp/content/text)

;; Ensure type is always serialized for text-content
(defmethod openrpc-server/interface:transform-result :around ((object text-content))
  (let ((result (call-next-method)))
    (when (hash-table-p result)
      (setf (gethash "type" result) (40ants-mcp/content/base:content-type object)))
    result))


(in-package :clack-sse)

(defvar *original-default-on-connect* (symbol-function 'default-on-connect))

(defun default-on-connect (&rest args)
  (handler-case
      (apply *original-default-on-connect* args)
    (error (c)
      (format *error-output* "Caught error in clack-sse:default-on-connect: ~A~%" c)
      (list 200
            (list :content-type "text/event-stream"
                  :cache-control "no-cache")))))

(defun serve-sse (stream-writer &key (on-connect #'default-on-connect))
  (lambda (env)
    (lambda (responder)
      (let ((response (funcall on-connect env)))
        (destructuring-bind (status headers &optional body) response
          (declare (ignore body))
          (let ((writer (funcall responder (list status headers))))
            (funcall stream-writer env (lack/util/writer-stream:make-writer-stream writer))))))))

(in-package :40ants-mcp/http-transport)

(setf *sse-handler* (clack-sse:serve-sse 'sse-stream-writer))


(defmethod 40ants-mcp/transport/base:start-loop ((transport http-transport) message-handler)
  "Start the HTTP server and begin processing requests.
   Monkey-patched to listen on 0.0.0.0 for Cloud Run."
  (log:info "Starting HTTP transport on port" (transport-port transport))
  (setf (transport-message-handler transport) message-handler)
  
  ;; Start the server
  (setf (transport-server transport)
        (clack:clackup (transport-lack-app transport)
                       :server :hunchentoot
                       :address "0.0.0.0"
                       :port (transport-port transport)
                       :use-thread nil)))

(defun handle-request (transport env)
  "Handle an incoming HTTP request.
   Monkey-patched to support root path '/'"
  (let ((path-info (getf env :path-info))
        (method (getf env :request-method)))
    (labels ((return-error-response (&key (code 500) (message "Internal Server Error"))
               (let ((error-response (serapeum:dict
                                      "jsonrpc" "2.0"
                                      "error" (serapeum:dict "code" code
                                                    "message" message))))
                 (return-from handle-request
                   (list 500
                         (list :content-type "application/json"
                               :mcp-protocol-version *protocol-version*)
                         (list (with-output-to-string (s)
                                 (yason:encode error-response s)))))))
             (parse-body (request)
               (handler-bind ((error (lambda (e)
                                       (log:error "Error processing request:" e)
                                       (return-error-response))))
                 (log4cl-extras/error:with-log-unhandled ()
                   (let* ((raw-body (lack/request:request-content request))
                          (body-string (when raw-body
                                         (babel:octets-to-string raw-body))))
                     (values body-string))))))
      (cond
        ((and (eq method :GET)
              (or (string= path-info "/") (string= path-info "/mcp")))
         (log:info "Responding with event stream" method path-info)
         
         (funcall *sse-handler* env))
        
        ;; Handle POST requests to / or /mcp endpoint
        ((and (eq method :POST)
              (or (string= path-info "/") (string= path-info "/mcp")))
         (let* ((request (let ((http-body::*content-type-map* nil))
                           ;; This call can signal SB-KERNEL:CASE-FAILURE if JSON will be invalid
                           ;; thus we set http-body::*content-type-map* to NIL and will parse
                           ;; body later in the transport-message-handler:
                           (lack/request:make-request env)))
                (body-string (parse-body request)))
           
           (log:info "Processing request" body-string)
           
           (handler-bind ((error (lambda (e)
                                   (log:error "Error in message handler:" e)
                                   (return-error-response))))
             (log4cl-extras/error:with-log-unhandled ()
               (let ((response (funcall (transport-message-handler transport)
                                        body-string)))
                 (log:info "Handler response:" response)
                 (log:info "Handler response type:" (type-of response))
                 (cond
                   ;; For notifications (no id), return 202 Accepted
                   ((null response)
                    (log:info "Handling notification")
                    (list 202
                          (list :content-type "application/json"
                                :mcp-protocol-version *protocol-version*)
                          nil))

                   ;; For other responses
                   ((typep response 'string)
                    (log:info "Handling string response")
                    (list 200
                          (list :content-type "application/json"
                                :mcp-protocol-version *protocol-version*)
                          (list response)))
                   
                   ((consp response)
                    (log:info "Handling a custom response" response)
                    (destructuring-bind (code headers body)
                        response
                      (unless (getf headers :mcp-protocol-version)
                        (setf (getf headers :mcp-protocol-version)
                              *protocol-version*))
                      (list code
                            headers
                            (uiop:ensure-list body))))
                   (t
                    (log:info "Unknown response type")
                    (return-error-response :code 500 :message "Unknown response type"))))))))
        ;; Return 404 for unknown paths
        (t
         (log:warn "Route not found" method path-info)
         (list 404
               (list :content-type "text/plain")
               '("Not Found")))))))


(in-package :mcp-server)

;;; Custom API Definition to separate user tools from protocol methods
(openrpc-server:define-api (user-tools :title "User Tools"))

;;; Business Logic

(defun get-greeting (name)
  (format nil "Hello, ~A!" name))

;;; Tool Definitions

;; Exposed on user-tools API
(define-tool (user-tools greet)
    (param)
  (:summary "Get a greeting from a local stdio server.")
  (:result (serapeum:soft-list-of 40ants-mcp/content/text:text-content))
  (:param param string "The name to greet.")
  (log-debug "Executed greet tool" (alexandria:plist-hash-table (list "param" param)))
  (list (make-instance '40ants-mcp/content/text:text-content
                       :text (get-greeting param))))

;; Sample tool requested by user
(define-tool (user-tools add) (a b)
  (:summary "Adds two numbers and returns the result.")
  (:param a integer "First number to add.")
  (:param b integer "Second number to add.")
  (:result (serapeum:soft-list-of 40ants-mcp/content/text:text-content))
  (list (make-instance '40ants-mcp/content/text:text-content
                      :text (format nil "The sum of ~A and ~A is: ~A"
                                  a b (+ a b)))))

;;; Main Entry Point

(defun main (&rest argv)
  (declare (ignore argv))
  ;; Disable the debugger to ensure we don't break stdio with debug info
  (setf *debugger-hook* (lambda (c h)
                          (declare (ignore h))
                          (log-json "ERROR" "Unhandled condition"
                                    (alexandria:plist-hash-table
                                     (list "condition" (format nil "~A" c))))
                          (uiop:quit 1)))
  
  (handler-case
      (handler-bind ((warning (lambda (c)
                                (log-json "WARN" (format nil "~A" c))
                                (muffle-warning c))))
        (let ((port (if (uiop:getenv "PORT")
                        (parse-integer (uiop:getenv "PORT"))
                        8080)))
          (log-info (format nil "Starting MCP server on port ~A..." port))
          ;; Start the server using HTTP transport, passing ONLY user-tools
          ;; This ensures tools/list only returns tools from user-tools API.
          ;; Protocol methods (initialize, etc.) are implicitly handled by the server instance.
          (start-server user-tools :transport :http :port port)))
    (error (c)
      (log-json "ERROR" "Fatal error in main loop"
                (alexandria:plist-hash-table
                 (list "error" (format nil "~A" c))))
      (uiop:quit 1))
    (sb-sys:interactive-interrupt ()
      (log-info "Exiting via interrupt...")
      (uiop:quit 0))))