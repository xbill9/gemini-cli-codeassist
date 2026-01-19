(load (merge-pathnames "quicklisp/setup.lisp" (user-homedir-pathname)))
(ql:quickload :40ants-mcp :silent t)
(format t "Value of MCP-SERVER: ~A~%" (symbol-value (find-symbol "MCP-SERVER" :40ants-mcp/server/definition)))
(uiop:quit)