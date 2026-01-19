(load "setup.lisp")
(asdf:load-asd (merge-pathnames "firestore-stdio-lisp.asd" (uiop:getcwd)))
(ql:quickload :firestore-stdio-lisp :silent t)

(in-package :mcp-server)

;; Try to find where tools are stored.
;; Based on logs: 40ANTS-MCP/SERVER/DEFINITION::TOOLS-COLLECTIONS

(let* ((server-class (find-symbol "MCP-SERVER" :40ants-mcp/server/definition))
       (tools-slot (find-symbol "TOOLS-COLLECTIONS" :40ants-mcp/server/definition)))
  (format t "Server Class: ~A~%" server-class)
  (format t "Tools Slot: ~A~%" tools-slot)
  
  ;; Inspect the tools map if possible.
  ;; The tools seem to be registered globally or on a singleton?
  ;; define-tool macro might register them.
  
  ;; Let's try to access the global variable or method that lists tools.
  ;; The log mentions 40ANTS-MCP/SERVER/API/TOOLS/LIST::TOOLS/LIST
  
  (let ((tools-list-fn (find-symbol "TOOLS/LIST" :40ants-mcp/server/api/tools/list)))
     (format t "Tools/List function: ~A~%" tools-list-fn))

  ;; Inspect API methods
  ;; The *CURRENT-API* might not be bound unless we are inside a request or define-api context.
  ;; But maybe there is a global API defined by 40ants-mcp.
  
  ;; Let's iterate over all symbols in 40ants-mcp/server/api/tools/list to find the tool name
  (let ((pkg (find-package :40ants-mcp/server/api/tools/list)))
     (when pkg
       (format t "Package 40ants-mcp/server/api/tools/list found.~%")
       (do-symbols (s pkg)
         (when (string-equal (symbol-name s) "TOOLS/LIST")
           (format t "Found symbol: ~A~%" s)
           ;; Check if it has properties related to openrpc
           ;; The macro might store info in plist
           (format t "Plist: ~A~%" (symbol-plist s))
         )
       )
     )
  )

  ;; Also check OPENRPC-SERVER/INTERFACE::*METHOD-REGISTRY* or similar if it exists?
  ;; Based on symbols: OPENRPC-SERVER:API-METHODS
  
  ;; Maybe we can find the API object in 40ants-mcp variables.
  (do-symbols (s :40ants-mcp/server/definition)
     (when (search "API" (symbol-name s))
        (format t "Symbol with API in name: ~A~%" s)
        (when (boundp s)
           (format t "  Value: ~A~%" (symbol-value s)))
     )
  )
)

(uiop:quit)
