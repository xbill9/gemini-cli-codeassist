(load (merge-pathnames "quicklisp/setup.lisp" (user-homedir-pathname)))
(ql:quickload :yason :silent t)
(handler-case
    (yason:with-output-to-string* ()
      (format t "Result: ~A~%" (yason:encode "test" nil)))
  (error (c)
    (format t "Caught error: ~A~%" c)))