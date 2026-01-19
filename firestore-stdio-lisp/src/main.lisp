(in-package :firestore-stdio-lisp)

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


(in-package :firestore-stdio-lisp)

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

(define-tool (user-tools add) (a b)
  (:summary "Adds two numbers and returns the result.")
  (:param a integer "First number to add.")
  (:param b integer "Second number to add.")
  (:result (serapeum:soft-list-of 40ants-mcp/content/text:text-content))
  (list (make-instance '40ants-mcp/content/text:text-content
                      :text (format nil "The sum of ~A and ~A is: ~A"
                                  a b (+ a b)))))

(define-tool (user-tools get_root) ()
  (:summary "Get a greeting from the Cymbal Superstore Inventory API.")
  (:result (serapeum:soft-list-of 40ants-mcp/content/text:text-content))
  (list (make-instance '40ants-mcp/content/text:text-content
                       :text "🍎 Hello! This is the Cymbal Superstore Inventory API.")))

(define-tool (user-tools check_db) ()
  (:summary "Checks if the inventory database is running.")
  (:result (serapeum:soft-list-of 40ants-mcp/content/text:text-content))
  (list (make-instance '40ants-mcp/content/text:text-content
                       :text (format nil "Database running: ~A"
                                     (firestore-stdio-lisp.inventory:db-running-p)))))

(define-tool (user-tools seed) ()
  (:summary "Seed the inventory database with products.")
  (:result (serapeum:soft-list-of 40ants-mcp/content/text:text-content))
  (if (not (firestore-stdio-lisp.inventory:db-running-p))
      (progn
        ;; Try to initialize if not running, similar to how TS code checks but also init logic
        (firestore-stdio-lisp.inventory:ensure-db-running)
        (if (not (firestore-stdio-lisp.inventory:db-running-p))
            (error "Inventory database is not running.")
            (progn
              (firestore-stdio-lisp.inventory:seed-database)
              (list (make-instance '40ants-mcp/content/text:text-content
                                   :text "Database seeded successfully.")))))
      (progn
        (firestore-stdio-lisp.inventory:seed-database)
        (list (make-instance '40ants-mcp/content/text:text-content
                             :text "Database seeded successfully.")))))

(define-tool (user-tools reset) ()
  (:summary "Clears all products from the inventory database.")
  (:result (serapeum:soft-list-of 40ants-mcp/content/text:text-content))
  (if (not (firestore-stdio-lisp.inventory:db-running-p))
       (error "Inventory database is not running.")
       (progn
         (firestore-stdio-lisp.inventory:reset-database)
         (list (make-instance '40ants-mcp/content/text:text-content
                              :text "Database reset successfully.")))))

(define-tool (user-tools get_products) ()
  (:summary "Get a list of all products from the inventory database")
  (:result (serapeum:soft-list-of 40ants-mcp/content/text:text-content))
  (if (not (firestore-stdio-lisp.inventory:db-running-p))
      (error "Inventory database is not running.")
      (let ((products (firestore-stdio-lisp.inventory:get-all-products)))
        (list (make-instance '40ants-mcp/content/text:text-content
                             :text (with-output-to-string (s)
                                     (yason:encode products s)))))))

(define-tool (user-tools get_product_by_id) (id)
  (:summary "Get a single product from the inventory database by its ID")
  (:param id string "The ID of the product to get")
  (:result (serapeum:soft-list-of 40ants-mcp/content/text:text-content))
  (if (not (firestore-stdio-lisp.inventory:db-running-p))
      (error "Inventory database is not running.")
      (let ((product (firestore-stdio-lisp.inventory:get-product-by-id id)))
        (if product
            (list (make-instance '40ants-mcp/content/text:text-content
                                 :text (with-output-to-string (s)
                                         (yason:encode product s))))
            (error "Product not found.")))))

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
        ;; Initialize Firestore
        (firestore-stdio-lisp.inventory:ensure-db-running)

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