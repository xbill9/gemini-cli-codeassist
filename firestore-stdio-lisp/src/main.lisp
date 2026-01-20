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
  (mcp-server::log-info "Called tool" (serapeum:dict "name" name))
  
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
                                    (40ants-mcp/server/errors:tool-error-content condition))))
         (error (condition)
           (log-json "ERROR" "Error during tool execution"
                     (serapeum:dict "tool" name
                                    "error" (format nil "~A" condition)))
           (make-instance 'tool-call-response
                          :is-error t
                          :content (list
                                    (make-instance '40ants-mcp/content/text:text-content
                                                   :text (format nil "Error executing tool \"~A\": ~A"
                                                                 name condition)))))))
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



(defvar *db-running* nil)



(defun get-greeting (name)

  "Returns a simple greeting string for the given NAME."

  (format nil "Hello, ~A!" name))



(defun check-db ()

  "Checks if the inventory database is reachable."

  (handler-case

      (progn

        (get-adc-token)

        (setf *db-running* t)

        t)

    (error (c)

      (log-json "ERROR" "Database check failed" (serapeum:dict "error" (format nil "~A" c)))

      (setf *db-running* nil)

      nil)))



;;; Tool Definitions



;; Exposed on user-tools API

(define-tool (user-tools greet)

    (param)

  (:summary "Get a greeting from a local stdio server.")

  (:result (serapeum:soft-list-of 40ants-mcp/content/text:text-content))

  (:param param string "The name to greet.")

  (log-debug "Executed greet tool" (serapeum:dict "param" param))

  (list (make-instance '40ants-mcp/content/text:text-content

                       :text (get-greeting param))))



(define-tool (user-tools get_products) ()



  (:summary "Get a list of all products from the inventory database")



  (:result (serapeum:soft-list-of 40ants-mcp/content/text:text-content))



  (log-debug "Executing get_products tool")



    (if (not *db-running*)



        (progn



          (log-debug "Database not running, signaling error")



          (error '40ants-mcp/server/errors:tool-error



                         :content (list (make-instance '40ants-mcp/content/text:text-content



                                                      :text "Inventory database is not running."))))



      (let ((products (get-products)))



        (log-debug "Fetched products, encoding to JSON" (serapeum:dict "count" (length products)))



        (let ((json (yason:with-output-to-string* ()



                      (yason:encode products))))



          (log-debug "Encoded products to JSON" (serapeum:dict "length" (length json)))



          (list (make-instance '40ants-mcp/content/text:text-content



                               :text json))))))



(define-tool (user-tools get_product_by_id) (id)

  (:summary "Get a single product from the inventory database by its ID")

  (:param id string "The ID of the product to get")

  (:result (serapeum:soft-list-of 40ants-mcp/content/text:text-content))

  (if (not *db-running*)
      (error '40ants-mcp/server/errors:tool-error
                     :content (list (make-instance '40ants-mcp/content/text:text-content
                                                  :text "Inventory database is not running.")))
      (let ((product (get-product-by-id id)))
        (if product
            (list (make-instance '40ants-mcp/content/text:text-content
                                 :text (yason:with-output-to-string* ()
                                         (yason:encode product))))
            (error '40ants-mcp/server/errors:tool-error
                           :content (list (make-instance '40ants-mcp/content/text:text-content
                                                        :text "Product not found.")))))))



(define-tool (user-tools seed) ()

  (:summary "Seed the inventory database with products.")

  (:result (serapeum:soft-list-of 40ants-mcp/content/text:text-content))

  (if (not *db-running*)
      (error '40ants-mcp/server/errors:tool-error
                     :content (list (make-instance '40ants-mcp/content/text:text-content
                                                  :text "Inventory database is not running.")))
      (progn
        (seed-inventory)
        (list (make-instance '40ants-mcp/content/text:text-content
                             :text "Database seeded successfully.")))))



(define-tool (user-tools reset) ()

  (:summary "Clears all products from the inventory database.")

  (:result (serapeum:soft-list-of 40ants-mcp/content/text:text-content))

  (if (not *db-running*)
      (error '40ants-mcp/server/errors:tool-error
                     :content (list (make-instance '40ants-mcp/content/text:text-content
                                                  :text "Inventory database is not running.")))
      (progn
        (clean-inventory)
        (list (make-instance '40ants-mcp/content/text:text-content
                             :text "Database reset successfully.")))))



(define-tool (user-tools get_root) ()

  (:summary "Get a greeting from the Cymbal Superstore Inventory API.")

  (:result (serapeum:soft-list-of 40ants-mcp/content/text:text-content))

  (list (make-instance '40ants-mcp/content/text:text-content

                       :text "🍎 Hello! This is the Cymbal Superstore Inventory API.")))



(define-tool (user-tools check_db) ()

  (:summary "Checks if the inventory database is running.")

  (:result (serapeum:soft-list-of 40ants-mcp/content/text:text-content))

  (let ((is-running (check-db)))

    (list (make-instance '40ants-mcp/content/text:text-content

                         :text (format nil "Database running: ~A" is-running)))))



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



  "Main entry point for the MCP server. Disables the debugger and starts the stdio server."



  (declare (ignore argv))







  ;; Redirect all standard output to stderr by default to avoid corrupting MCP protocol



  (let ((stdout *standard-output*))



    (setf *standard-output* *error-output*)



    (setf *terminal-io* (make-two-way-stream *standard-input* *error-output*))







    ;; Configure log4cl if it's loaded to use stderr



    (when (find-package :log4cl)



      (ignore-errors



       (let ((root-logger (uiop:symbol-call :log4cl :make-log-hierarchy)))



         (uiop:symbol-call :log4cl :remove-all-appenders root-logger)



         (uiop:symbol-call :log4cl :add-appender root-logger



                           (make-instance (uiop:find-symbol* :console-appender :log4cl)



                                          :stream *error-output*)))))







        ;; Initialize Firestore connection







    







        (setf yason:*symbol-encoder* #'symbol-name)







    







        (check-db)







        ;; Disable the debugger to ensure we don't break stdio with debug info







        (setf *debugger-hook* (lambda (c h)







                                (declare (ignore h))







                                ;; Prevent recursion if log-json fails







                                (setf *debugger-hook* nil)







                                (ignore-errors







                                 (log-json "ERROR" "Unhandled condition"







                                           (serapeum:dict "condition" (format nil "~A" c))))







                                (uiop:quit 1)))







    







        (handler-case







            (handler-bind ((warning (lambda (c)







                                      (ignore-errors







                                       (log-json "WARN" (format nil "~A" c)))







                                      (muffle-warning c))))







    



          (log-info "Starting MCP server...")



          ;; Start the server using stdio transport, passing ONLY user-tools



          ;; We bind *standard-output* back to the real stdout for the protocol.



          (let ((*standard-output* stdout))



            (start-server user-tools :transport :stdio)))



      (error (c)



        (log-json "ERROR" "Fatal error in main loop"



                  (serapeum:dict "error" (format nil "~A" c)))



        (uiop:quit 1))



      (sb-sys:interactive-interrupt ()



        (log-info "Exiting via interrupt...")



        (uiop:quit 0)))))
