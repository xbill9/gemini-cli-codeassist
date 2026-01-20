(load (merge-pathnames "quicklisp/setup.lisp" (user-homedir-pathname)))
(ql:quickload :yason :silent t)
(handler-case
    (let ((out (yason:with-output-to-string* ()
                 (yason:encode :get))))
      (format t "Result for :get -> ~A~%" out))
  (error (c)
    (format t "Caught error for :get: ~A~%" c)))
