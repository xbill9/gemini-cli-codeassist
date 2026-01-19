(defpackage :firestore-stdio-lisp.firestore
  (:use :cl)
  (:import-from :dexador)
  (:import-from :yason)
  (:import-from :uiop)
  (:import-from :serapeum)
  (:import-from :alexandria)
  (:import-from :local-time)
  (:import-from :firestore-stdio-lisp.logger :log-info :log-error :log-debug)
  (:export :init-firestore
           :get-collection
           :get-document
           :create-document
           :update-document
           :delete-document
           :firestore-error
           :*firestore-project-id*))

(in-package :firestore-stdio-lisp.firestore)

(defvar *firestore-project-id* nil)
(defvar *access-token* nil)
(defvar *base-url* "https://firestore.googleapis.com/v1")

;;; --- Authentication & Config ---

(defun get-gcloud-token ()
  "Attempts to get a token via gcloud CLI."
  (handler-case
      (string-trim '(#\Space #\Newline #\Return)
                   (uiop:run-program '("gcloud" "auth" "print-access-token")
                                     :output :string))
    (error (e)
      (log-error "Failed to get gcloud token" e)
      nil)))

(defun get-gcloud-project ()
  "Attempts to get the project ID via gcloud CLI."
  (handler-case
      (string-trim '(#\Space #\Newline #\Return)
                   (uiop:run-program '("gcloud" "config" "get-value" "project")
                                     :output :string))
    (error (e)
      (log-error "Failed to get gcloud project" e)
      nil)))

(defun init-firestore ()
  "Initializes Firestore configuration from environment or gcloud."
  (setf *access-token* (or (uiop:getenv "ACCESS_TOKEN")
                           (get-gcloud-token)))
  (setf *firestore-project-id* (or (uiop:getenv "GCP_PROJECT")
                                   (uiop:getenv "GOOGLE_CLOUD_PROJECT")
                                   (get-gcloud-project)))
  
  (unless *access-token*
    (log-error "Could not obtain Access Token. Please set ACCESS_TOKEN or ensure gcloud is authenticated."))
  (unless *firestore-project-id*
    (log-error "Could not determine GCP Project ID. Please set GCP_PROJECT or ensure gcloud config is set."))
  
  (and *access-token* *firestore-project-id*))

;;; --- Data Conversion ---

(defun to-firestore-value (value)
  "Converts a Lisp value to a Firestore Value object."
  (cond
    ((null value) (yason:parse "{\"nullValue\": null}"))
    ((eq value t) (yason:parse "{\"booleanValue\": true}"))
    ((eq value :false) (yason:parse "{\"booleanValue\": false}"))
    ((stringp value) (alexandria:plist-hash-table (list "stringValue" value) :test 'equal))
    ((integerp value) (alexandria:plist-hash-table (list "integerValue" (format nil "~D" value)) :test 'equal)) ;; Firestore requires int64 as string
    ((floatp value) (alexandria:plist-hash-table (list "doubleValue" value) :test 'equal))
    ((typep value 'local-time:timestamp)
     (alexandria:plist-hash-table (list "timestampValue" (local-time:format-rfc3339-timestring nil value)) :test 'equal))
    (t (error "Unsupported type for Firestore conversion: ~A" (type-of value)))))

(defun from-firestore-value (fv)
  "Converts a Firestore Value object to a Lisp value."
  ;; fv is a hash-table like {"stringValue": "foo"}
  (cond
    ((gethash "stringValue" fv) (gethash "stringValue" fv))
    ((gethash "integerValue" fv) (parse-integer (gethash "integerValue" fv)))
    ((gethash "doubleValue" fv) (gethash "doubleValue" fv))
    ((gethash "booleanValue" fv) (gethash "booleanValue" fv))
    ((gethash "timestampValue" fv) (local-time:parse-rfc3339-timestring (gethash "timestampValue" fv)))
    ((gethash "nullValue" fv) nil)
    (t fv)))

(defun fields-to-hash (fields)
  "Converts a Firestore 'fields' object to a Lisp hash-table."
  (let ((result (make-hash-table :test 'equal)))
    (maphash (lambda (k v)
               (setf (gethash k result) (from-firestore-value v)))
             fields)
    result))

(defun hash-to-fields (hash)
  "Converts a Lisp hash-table to a Firestore 'fields' object."
  (let ((fields (make-hash-table :test 'equal)))
    (maphash (lambda (k v)
               (setf (gethash k fields) (to-firestore-value v)))
             hash)
    fields))

;;; --- API Methods ---

(defun make-firestore-url (path)
  (format nil "~A/projects/~A/databases/(default)/documents/~A"
          *base-url* *firestore-project-id* path))

(defun auth-headers ()
  (list :authorization (format nil "Bearer ~A" *access-token*)
        :content-type "application/json"))

(defun get-collection (collection-id)
  "Fetches all documents in a collection."
  (handler-case
      (let* ((response (dexador:get (make-firestore-url collection-id)
                                    :headers (auth-headers)))
             (json (yason:parse response))
             (documents (gethash "documents" json)))
        (mapcar (lambda (doc)
                  (let ((fields (gethash "fields" doc))
                        (name (gethash "name" doc)))
                    ;; name is like projects/.../documents/collection/docId
                    (let ((id (car (last (uiop:split-string name :separator "/")))))
                      (let ((data (fields-to-hash fields)))
                        (setf (gethash "id" data) id)
                        data))))
                documents))
    (dexador:http-request-failed (e)
      (log-error "Firestore request failed" (dexador:response-body e))
      nil)))

(defun get-document (collection-id doc-id)
  "Fetches a single document."
  (handler-case
      (let* ((response (dexador:get (format nil "~A/~A" (make-firestore-url collection-id) doc-id)
                                    :headers (auth-headers)))
             (doc (yason:parse response)))
        (let ((fields (gethash "fields" doc)))
          (when fields
            (let ((data (fields-to-hash fields)))
              (setf (gethash "id" data) doc-id)
              data))))
    (dexador:http-request-not-found ()
      nil)
    (dexador:http-request-failed (e)
      (log-error "Firestore request failed" (dexador:response-body e))
      nil)))

(defun create-document (collection-id data &optional doc-id)
  "Creates or overwrites a document."
  ;; If doc-id is provided, use PATCH (upsert) or PUT.
  ;; Firestore REST API 'createDocument' is a POST to collection.
  ;; 'patch' is PATCH to document URL.
  (let ((fields (hash-to-fields data))
        (payload (make-hash-table :test 'equal)))
    (setf (gethash "fields" payload) fields)
    
    (handler-case
        (if doc-id
            ;; Use PATCH to update/upsert
            (dexador:patch (format nil "~A/~A" (make-firestore-url collection-id) doc-id)
                           :headers (auth-headers)
                           :content (with-output-to-string (s) (yason:encode payload s)))
            ;; Use POST to create new with auto-id
            (dexador:post (make-firestore-url collection-id)
                          :headers (auth-headers)
                          :content (with-output-to-string (s) (yason:encode payload s))))
      (dexador:http-request-failed (e)
        (log-error "Firestore create/update failed" (dexador:response-body e))
        (error "Firestore error: ~A" e)))))

(defun delete-document (collection-id doc-id)
  (handler-case
      (dexador:delete (format nil "~A/~A" (make-firestore-url collection-id) doc-id)
                      :headers (auth-headers))
    (dexador:http-request-failed (e)
      (log-error "Firestore delete failed" (dexador:response-body e))
      nil)))
