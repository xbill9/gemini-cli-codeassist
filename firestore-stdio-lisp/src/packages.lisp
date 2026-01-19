(in-package :cl-user)

(defpackage :firestore-stdio-lisp
  (:use :cl)
  (:import-from :40ants-mcp/tools
                :define-tool)
  (:import-from :40ants-mcp/server/definition
                :start-server
                :mcp-server)
  (:import-from :firestore-stdio-lisp.logger
                :log-info
                :log-error
                :log-debug
                :log-json)
  (:export :main
           :get-greeting))
