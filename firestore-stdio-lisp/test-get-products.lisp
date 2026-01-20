(load (merge-pathnames "quicklisp/setup.lisp" (user-homedir-pathname)))
(load "firestore-stdio-lisp.asd")
(ql:quickload :firestore-stdio-lisp :silent t)

(in-package :mcp-server)

(handler-case
    (let ((products (get-products)))
      (format t "Successfully fetched ~D products.~%" (length products))
      (let ((json (yason:with-output-to-string* ()
                    (yason:encode products))))
        (format t "Successfully encoded products to JSON (length: ~D)~%" (length json)))
      (dolist (p (subseq products 0 (min 3 (length products))))
        (format t "Product: ~A (Price: ~A)~%" (gethash "name" p) (gethash "price" p))))
  (error (c)
    (format t "Error: ~A~%" c)
    (sb-debug:print-backtrace)
    (uiop:quit 1)))

(uiop:quit)
