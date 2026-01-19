(load (merge-pathnames "quicklisp/setup.lisp" (user-homedir-pathname)))
(push (uiop:getcwd) asdf:*central-registry*)
(ql:quickload :mcp-https-lisp)

(sb-ext:save-lisp-and-die "mcp-server"
                         :executable t
                         :toplevel 'mcp-server:main)
