(in-package :mcp-server)

(defun current-timestamp-string ()
  "Returns the current UTC time as a formatted ISO-8601 string including microseconds."
  (local-time:format-timestring nil (local-time:now)
                                :format '(:year "-" :month "-" :day "T"
                                          :hour ":" :min ":" :sec "." :usec
                                          :timezone)))

(defun log-json (level message &optional context)
  "Logs a message to STDERR in JSON format."
  (ignore-errors
   (let ((log-entry (serapeum:dict "timestamp" (current-timestamp-string)
                                   "level" level
                                   "message" message)))
     (when context
       (setf (gethash "context" log-entry) context))
     (yason:encode log-entry *error-output*)
     (format *error-output* "~%")
     (finish-output *error-output*))))

(defun log-info (message &optional context)
  "Logs an informational message to STDERR as JSON."
  (log-json "INFO" message context))

(defun log-debug (message &optional context)
  "Logs a debug message to STDERR as JSON."
  (log-json "DEBUG" message context))
