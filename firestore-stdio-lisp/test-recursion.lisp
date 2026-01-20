(load (merge-pathnames "quicklisp/setup.lisp" (user-homedir-pathname)))
(ql:quickload '(:yason :serapeum :local-time) :silent t)

(defun log-json (message context)
  (let ((log-entry (serapeum:dict "message" message "context" context)))
    (yason:encode log-entry *error-output*)
    (terpri *error-output*)
    (finish-output *error-output*)))

(setf *debugger-hook* (lambda (c h)
                        (declare (ignore h))
                        (format *error-output* "In debugger hook caught: ~A~%" c)
                        (log-json "Error in debugger" (serapeum:dict "error" (format nil "~A" c)))
                        (uiop:quit 1)))

(format t "Triggering error...~%")
;; yason:encode does NOT know how to encode local-time:timestamp by default
(log-json "Test" (serapeum:dict "timestamp" (local-time:now)))