(in-package :mcp-server)

(defvar *firestore-project-id* nil)
(defvar *firestore-access-token* nil)
(defvar *token-expiry* 0)

(defvar *project-id-checked* nil)

(defun get-google-project-id ()
  "Attempts to get the Google Cloud Project ID from environment variables or metadata service."
  (unless *project-id-checked*
    (setf *firestore-project-id*
          (or (uiop:getenv "GOOGLE_CLOUD_PROJECT")
              (uiop:getenv "GCP_PROJECT")
              (handler-case
                  (dex:get "http://metadata.google.internal/computeMetadata/v1/project/project-id"
                           :headers '(("Metadata-Flavor" . "Google"))
                           :connect-timeout 2
                           :read-timeout 2)
                (error () nil))))
    (setf *project-id-checked* t))
  *firestore-project-id*)

(defun get-adc-token ()
  "Simple ADC token fetcher. Only handles metadata service or GOOGLE_APPLICATION_CREDENTIALS if it's a service account."
  (let ((now (get-universal-time)))
    (if (and *firestore-access-token* (< now (- *token-expiry* 60)))
        *firestore-access-token*
        (let ((token-info
                (handler-case
                    (let ((response (dex:get "http://metadata.google.internal/computeMetadata/v1/instance/service-accounts/default/token"
                                             :headers '(("Metadata-Flavor" . "Google"))
                                             :connect-timeout 2
                                             :read-timeout 2)))
                      (yason:parse response))
                  (error ()
                    (log-info "Metadata service not available or failed, checking GOOGLE_APPLICATION_CREDENTIALS")
                    (handler-case
                        (progn
                          (log-debug "Attempting to get token from gcloud")
                          (let ((token (uiop:run-program "gcloud auth print-access-token" :output :string)))
                            (log-debug "Successfully got token from gcloud")
                            (let ((alist (list (cons "access_token" (string-trim '(#\Newline #\Space) token))
                                               (cons "expires_in" 3600))))
                              alist)))
                      (error ()
                        (log-json "ERROR" "Failed to get ADC token from gcloud")
                        nil))))))
          (when token-info
            (flet ((get-val (key)
                     (if (hash-table-p token-info)
                         (gethash key token-info)
                         (alexandria:assoc-value token-info key :test 'equal))))
              (setf *firestore-access-token* (get-val "access_token")
                    *token-expiry* (+ now (or (get-val "expires_in") 3600)))
              *firestore-access-token*))))))

(defun firestore-request (method path &key content params)
  (let ((token (get-adc-token))
        (project-id (or *firestore-project-id* (setf *firestore-project-id* (get-google-project-id)))))
    (unless project-id (error "Project ID not found"))
    (unless token (error "Access token not found"))
    (let ((url (format nil "https://firestore.googleapis.com/v1/projects/~A/databases/(default)/documents~A"
                       project-id path)))
      (log-debug "Sending Firestore request" (serapeum:dict "method" method "url" url))
      (handler-case
          (let ((response
                  (dex:request url
                               :method method
                               :headers `(("Authorization" . ,(format nil "Bearer ~A" token))
                                          ("Content-Type" . "application/json"))
                               :connect-timeout 5
                               :read-timeout 10
                               :content (when content (yason:with-output-to-string* () (yason:encode content))))))
            (log-debug "Received Firestore response" (serapeum:dict "status" 200))
            (yason:parse response))
        (dex:http-request-failed (e)
          (log-json "ERROR" "Firestore request failed"
                    (serapeum:dict "url" url
                                   "status" (dex:response-status e)
                                   "body" (dex:response-body e)))
          (error e))))))

;;; Firestore Value Conversion

(defun to-firestore-value (val)
  (cond
    ((stringp val) (serapeum:dict "stringValue" val))
    ((integerp val) (serapeum:dict "integerValue" (format nil "~D" val)))
    ((numberp val) (serapeum:dict "doubleValue" val))
    ((typep val 'local-time:timestamp) (serapeum:dict "timestampValue" (local-time:format-rfc3339-timestring nil val)))
    ((null val) (serapeum:dict "nullValue" nil))
    (t (serapeum:dict "stringValue" (format nil "~A" val)))))

(defun from-firestore-value (val)
  (when (hash-table-p val)
    (cond
      ((gethash "stringValue" val))
      ((gethash "integerValue" val) (parse-integer (gethash "integerValue" val)))
      ((gethash "doubleValue" val))
      ((gethash "timestampValue" val) (local-time:parse-rfc3339-timestring (gethash "timestampValue" val)))
      ((gethash "booleanValue" val))
      (t nil))))

(defmethod yason:encode ((object local-time:timestamp) &optional (stream *standard-output*))
  (yason:encode (local-time:format-rfc3339-timestring nil object) stream))

(defun doc-to-product (doc)
  (let ((fields (gethash "fields" doc))
        (name (gethash "name" doc)))
    (serapeum:dict
     "id" (car (last (uiop:split-string name :separator '(#\/))))
     "name" (from-firestore-value (gethash "name" fields))
     "price" (from-firestore-value (gethash "price" fields))
     "quantity" (from-firestore-value (gethash "quantity" fields))
     "imgfile" (from-firestore-value (gethash "imgfile" fields))
     "timestamp" (from-firestore-value (gethash "timestamp" fields))
     "actualdateadded" (from-firestore-value (gethash "actualdateadded" fields)))))

(defun product-to-fields (product)
  (let ((fields (make-hash-table :test 'equal)))
    (maphash (lambda (k v)
               (unless (string= k "id")
                 (setf (gethash k fields) (to-firestore-value v))))
             product)
    fields))

;;; Business Logic

(defun get-products ()
  (let ((response (firestore-request :get "/inventory")))
    (mapcar #'doc-to-product (gethash "documents" response))))

(defun get-product-by-id (id)
  (handler-case
      (doc-to-product (firestore-request :get (format nil "/inventory/~A" id)))
    (error () nil)))

(defun add-or-update-product (product)
  (let* ((name (gethash "name" product))
         (query-response (firestore-request :post ":runQuery"
                                           :content (serapeum:dict
                                                     "structuredQuery"
                                                     (serapeum:dict
                                                      "from" (list (serapeum:dict "collectionId" "inventory"))
                                                      "where" (serapeum:dict
                                                               "fieldFilter"
                                                               (serapeum:dict
                                                                "field" (serapeum:dict "fieldPath" "name")
                                                                "op" "EQUAL"
                                                                "value" (serapeum:dict "stringValue" name))))))))
    (if (and query-response (gethash "document" (car query-response)))
        (let ((doc-name (gethash "name" (gethash "document" (car query-response)))))
          (firestore-request :patch (subseq doc-name (length (format nil "projects/~A/databases/(default)/documents" *firestore-project-id*)))
                            :content (serapeum:dict "fields" (product-to-fields product))))
        (firestore-request :post "/inventory"
                          :content (serapeum:dict "fields" (product-to-fields product))))))

(defun clean-inventory ()
  (let ((products (gethash "documents" (firestore-request :get "/inventory"))))
    (dolist (doc products)
      (let ((doc-name (gethash "name" doc)))
        (firestore-request :delete (subseq doc-name (length (format nil "projects/~A/databases/(default)/documents" *firestore-project-id*))))))))

(defun seed-inventory ()
  (let ((old-products '("Apples" "Bananas" "Milk" "Whole Wheat Bread" "Eggs" "Cheddar Cheese"
                        "Whole Chicken" "Rice" "Black Beans" "Bottled Water" "Apple Juice"
                        "Cola" "Coffee Beans" "Green Tea" "Watermelon" "Broccoli"
                        "Jasmine Rice" "Yogurt" "Beef" "Shrimp" "Walnuts"
                        "Sunflower Seeds" "Fresh Basil" "Cinnamon"))
        (recent-products '("Parmesan Crisps" "Pineapple Kombucha" "Maple Almond Butter"
                           "Mint Chocolate Cookies" "White Chocolate Caramel Corn" "Acai Smoothie Packs"
                           "Smores Cereal" "Peanut Butter and Jelly Cups"))
        (oos-products '("Wasabi Party Mix" "Jalapeno Seasoning")))

    (flet ((add-p (name qty days-ago)
             (let ((p (serapeum:dict
                       "name" name
                       "price" (+ 1 (random 10))
                       "quantity" qty
                       "imgfile" (format nil "product-images/~A.png"
                                         (remove #\Space (string-downcase name)))
                       "timestamp" (local-time:timestamp- (local-time:now) days-ago :day)
                       "actualdateadded" (local-time:now))))
               (log-info (format nil "Adding/Updating: ~A" name))
               (add-or-update-product p))))

      (dolist (name old-products)
        (add-p name (+ 1 (random 500)) (+ 30 (random 365))))
      (dolist (name recent-products)
        (add-p name (+ 1 (random 100)) (random 7)))
      (dolist (name oos-products)
        (add-p name 0 (random 7))))))