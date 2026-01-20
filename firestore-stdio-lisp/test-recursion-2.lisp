(load (merge-pathnames "quicklisp/setup.lisp" (user-homedir-pathname)))
(load "firestore-stdio-lisp.asd")
(ql:quickload :firestore-stdio-lisp :silent t)

(in-package :mcp-server)

(format t "Testing yason:encode with timestamp...~%")
(let ((ts (local-time:now)))
  (format t "Timestamp: ~A~%" ts)
  (let ((json (yason:with-output-to-string* ()
                (yason:encode ts))))
    (format t "JSON: ~A~%" json)))

(format t "Testing log-json with timestamp in context...~%")
(log-json "INFO" "Test message" (serapeum:dict "ts" (local-time:now)))
(format t "Done.~%")
