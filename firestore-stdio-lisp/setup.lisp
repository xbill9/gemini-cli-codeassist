(in-package :cl-user)

(require :asdf)

#-quicklisp
(let ((quicklisp-init (merge-pathnames "quicklisp/setup.lisp"
                                       (user-homedir-pathname))))
  (when (probe-file quicklisp-init)
    (load quicklisp-init)))

(unless (find-package :ql)
  (format t "Quicklisp not found. Installing...~%")
  (let ((ql-install-file (merge-pathnames "quicklisp.lisp" (uiop:getcwd))))
    (unless (probe-file ql-install-file)
      (format t "Downloading Quicklisp installer to ~A...~%" ql-install-file)
      (sb-ext:run-program "curl" (list "-O" "https://beta.quicklisp.org/quicklisp.lisp") :search t :output t))
    (load ql-install-file)
    (funcall (find-symbol "INSTALL" :quicklisp-quickstart)))
  (format t "Quicklisp installed.~%"))

;; Add 40ants ultralisp dist if not present
(unless (ql-dist:find-dist "ultralisp")
  (ql-dist:install-dist "http://dist.ultralisp.org/" :prompt nil))

;; Update all dists to ensure we have the latest metadata
(ql:update-all-dists :prompt nil)

;; Ensure dependencies are installed
;; Load jsonrpc first to ensure we get the version compatible with 40ants-mcp from Ultralisp
(ql:quickload :jsonrpc :silent nil)
(ql:quickload '(:40ants-mcp :serapeum :yason :local-time) :silent nil)

(format t "Dependencies installed successfully.~%")
