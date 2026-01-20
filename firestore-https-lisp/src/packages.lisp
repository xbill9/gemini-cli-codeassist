(in-package :cl-user)

(defpackage :mcp-server
  (:use :cl)
  (:import-from :40ants-mcp/tools
                :define-tool)
  (:import-from :40ants-mcp/server/definition
                :start-server
                :mcp-server)
  (:export :main
           :get-greeting))
