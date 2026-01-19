(defpackage :firestore-stdio-lisp.logger
  (:use :cl)
  (:import-from :local-time)
  (:import-from :yason)
  (:import-from :alexandria)
  (:export :log-info
           :log-error
           :log-debug
           :log-json))

(in-package :firestore-stdio-lisp.logger)

(defun current-timestamp-string ()
  (local-time:format-timestring nil (local-time:now)
                                :format '(:year "-" :month "-" :day "T"
                                          :hour ":" :min ":" :sec "." :usec
                                          :timezone)))

(defun log-json (level message &optional context)
  "Logs a message to STDERR in JSON format."
  (let ((log-entry (alexandria:plist-hash-table
                    (list "timestamp" (current-timestamp-string)
                          "level" level
                          "message" message)
                    :test 'equal)))
    (when context
      (setf (gethash "context" log-entry) context))
    (yason:encode log-entry *error-output*)
    (format *error-output* "~%")
    (finish-output *error-output*)))

(defun log-info (message &optional context)
  (log-json "INFO" message context))

(defun log-error (message &optional context)
  (log-json "ERROR" message context))

(defun log-debug (message &optional context)
  (log-json "DEBUG" message context))
