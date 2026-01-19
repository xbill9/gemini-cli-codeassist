(in-package :cl-user)

(format t "Loading Quicklisp...~%")
(load (merge-pathnames "quicklisp/setup.lisp" (user-homedir-pathname)))

(format t "Loading system definition...~%")
(load "firestore-stdio-lisp.asd")

(format t "Loading system...~%")
(ql:quickload :firestore-stdio-lisp)

(format t "Building binary firestore-stdio-lisp...~%")
(sb-ext:save-lisp-and-die "firestore-stdio-lisp"
 :toplevel #'firestore-stdio-lisp:main                          :executable t
                          :compression t)
