(load (merge-pathnames "quicklisp/setup.lisp" (user-homedir-pathname)))
(asdf:load-asd (merge-pathnames "mcp-https-lisp.asd" (uiop:getcwd)))
(ql:quickload :mcp-https-lisp :silent t)

(in-package :cl-user)

(defun investigate ()
  (format t "Scanning for bound variables...~%")
  
  (dolist (pkg-name '("OPENRPC-SERVER" "OPENRPC-SERVER/METHOD" "OPENRPC-SERVER/INTERFACE" "40ANTS-MCP/SERVER/DEFINITION"))
    (let ((pkg (find-package pkg-name)))
      (when pkg
        (do-symbols (s pkg)
          (when (and (boundp s)
                     (not (keywordp s))) ;; skip keywords if any
            (let ((val (symbol-value s)))
              ;; Heuristic: if it's a hash-table or list, it might be interesting
              (when (or (hash-table-p val) 
                        (and (listp val) val)
                        (typep val 'standard-object))
                (format t "Variable ~A::~A is bound. Value type: ~A~%" (package-name pkg) s (type-of val))
                (when (search "METHOD" (symbol-name s))
                   (format t "  -> Potential match! Value: ~A~%" val))
                (when (search "API" (symbol-name s))
                   (format t "  -> Potential match! Value: ~A~%" val))
              )
            )
          )
        )
      )
    )
  )

  ;; Also check for known symbols if any
  (let ((methods-sym (find-symbol "*METHODS*" :openrpc-server/method)))
    (when (and methods-sym (boundp methods-sym))
       (format t "*METHODS* is bound: ~A~%" (symbol-value methods-sym))))
)

(investigate)
(uiop:quit)