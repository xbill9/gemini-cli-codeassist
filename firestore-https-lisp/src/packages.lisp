(in-package :cl-user)

(defpackage :mcp-server.logger
  (:use :cl)
  (:export #:log-info
           #:log-debug
           #:log-json))

(defpackage :mcp-server.firestore
  (:use :cl)
  (:import-from :mcp-server.logger
                #:log-info
                #:log-debug)
  (:export #:init-firestore
           #:get-products
           #:get-product-by-id
           #:search-products
           #:seed-database
           #:reset-database
           #:*db-running*))

(defpackage :mcp-server
  (:use :cl)
  (:import-from :40ants-mcp/tools
                :define-tool)
  (:import-from :40ants-mcp/server/definition
                :start-server
                :mcp-server)
  (:import-from :mcp-server.firestore
                #:init-firestore
                #:get-products
                #:get-product-by-id
                #:search-products
                #:seed-database
                #:reset-database
                #:*db-running*)
  (:import-from :mcp-server.logger
                #:log-info
                #:log-debug
                #:log-json)
  (:export :main
           :get-greeting))
