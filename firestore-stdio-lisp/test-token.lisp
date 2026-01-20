(load "setup.lisp")
(load "firestore-stdio-lisp.asd")
(ql:quickload :firestore-stdio-lisp)

(in-package :mcp-server)

(format t "Testing get-google-project-id...~%")
(format t "Project ID: ~A~%" (get-google-project-id))

(format t "Testing get-adc-token...~%")
(let ((token (get-adc-token)))
  (if token
      (progn
        (format t "Token found: ~A...~%" (subseq token 0 10))
        (format t "Testing get-products...~%")
        (handler-case
            (let ((products (get-products)))
              (format t "Found ~D products.~%" (length products))
              (dolist (p (subseq products 0 (min (length products) 3)))
                (format t " - ~A ($~A)~%" (gethash "name" p) (gethash "price" p))))
          (error (c)
            (format t "get-products FAILED: ~A~%" c))))
      (format t "Token NOT found.~%")))
