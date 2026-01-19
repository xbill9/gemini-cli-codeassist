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
        (log-info "Starting MCP server...")
        ;; Start the server using stdio transport, passing ONLY user-tools
        ;; This ensures tools/list only returns tools from user-tools API.
        ;; Protocol methods (initialize, etc.) are implicitly handled by the server instance.
        (start-server user-tools :transport :stdio))
    (error (c)
      (log-json "ERROR" "Fatal error in main loop"
                (alexandria:plist-hash-table
                 (list "error" (format nil "~A" c))))
      (uiop:quit 1))
    (sb-sys:interactive-interrupt ()
      (log-info "Exiting via interrupt...")
      (uiop:quit 0))))