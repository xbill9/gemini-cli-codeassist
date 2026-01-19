(in-package :cl-user)

(load "setup.lisp")

(load "firestore-stdio-lisp.asd")

(ql:quickload :firestore-stdio-lisp)

(asdf:make :firestore-stdio-lisp)

(format t "Building binary mcp-server...~%")
(sb-ext:save-lisp-and-die "mcp-server"
                          :toplevel #'mcp-server:main
                          :executable t
                          :compression t)
