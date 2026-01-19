(load (merge-pathnames "quicklisp/setup.lisp" (user-homedir-pathname)))
(ql:quickload '(:40ants-mcp :sb-introspect) :silent t)
(format t "Lambda list for START-SERVER: ~A~%" 
        (sb-introspect:function-lambda-list (find-symbol "START-SERVER" :40ants-mcp/server/definition)))
(uiop:quit)