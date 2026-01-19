(in-package :cl-user)

(format t "Loading Quicklisp...~%")
(load (merge-pathnames "quicklisp/setup.lisp" (user-homedir-pathname)))

(format t "Loading system definition...~%")
(load "mcp-stdio-lisp.asd")

(format t "Loading system...~%")
(ql:quickload :mcp-stdio-lisp)

(format t "Building binary mcp-server...~%")
(sb-ext:save-lisp-and-die "mcp-server"
                          :toplevel #'mcp-server:main
                          :executable t
                          :compression t)
