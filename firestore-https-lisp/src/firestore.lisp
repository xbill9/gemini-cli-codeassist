(in-package :mcp-server.firestore)

(defvar *project-id* nil)
(defvar *db-running* nil)
(defvar *access-token* nil)
(defvar *token-expiration* 0)

(defun get-project-id ()
  (or *project-id*
      (setf *project-id* (uiop:getenv "GOOGLE_CLOUD_PROJECT"))))

(defun get-access-token ()
  (let ((now (get-universal-time)))
    (if (and *access-token* (> *token-expiration* now))
        *access-token*
        (refresh-access-token))))

(defun refresh-access-token ()
  (handler-case
      (let* ((response (dex:get "http://metadata.google.internal/computeMetadata/v1/instance/service-accounts/default/token"
                                :headers '(("Metadata-Flavor" . "Google"))
                                :connect-timeout 2
                                :read-timeout 2))
             (json (yason:parse response))
             (token (gethash "access_token" json))
             (expires-in (gethash "expires_in" json)))
        (setf *access-token* token)
        (setf *token-expiration* (+ (get-universal-time) expires-in -60)) ;; buffer
        token)
    (error (e)
      (log-info (format nil "Failed to get metadata token: ~A. Using environment variable or empty." e))
      (let ((token (uiop:getenv "GCP_ACCESS_TOKEN")))
        (if token
             (progn
               (setf *access-token* token)
               (setf *token-expiration* (+ (get-universal-time) 3600))
               token)
             (progn
               (log-info "No GCP Access Token available.")
               ""))))))

(defun firestore-base-url ()
  (format nil "https://firestore.googleapis.com/v1/projects/~A/databases/(default)/documents"
          (get-project-id)))

(defun firestore-request (method url &optional body)
  (let ((token (get-access-token)))
    (dex:request url
                 :method method
                 :headers `(("Authorization" . ,(format nil "Bearer ~A" token))
                            ("Content-Type" . "application/json"))
                 :content (when body (with-output-to-string (s) (yason:encode body s))))))

;;; Data Models

(defstruct product
  id name price quantity imgfile timestamp actualdateadded)

(defmethod yason:encode ((p product) &optional (stream *standard-output*))
  (yason:with-output (stream)
    (yason:with-object ()
      (yason:encode-object-element "id" (product-id p))
      (yason:encode-object-element "name" (product-name p))
      (yason:encode-object-element "price" (product-price p))
      (yason:encode-object-element "quantity" (product-quantity p))
      (yason:encode-object-element "imgfile" (product-imgfile p))
      (yason:encode-object-element "timestamp" (product-timestamp p))
      (yason:encode-object-element "actualdateadded" (product-actualdateadded p)))))

(defun parse-firestore-value (value-obj)
  (cond
    ((gethash "stringValue" value-obj) (gethash "stringValue" value-obj))
    ((gethash "integerValue" value-obj) (parse-integer (gethash "integerValue" value-obj)))
    ((gethash "doubleValue" value-obj) (gethash "doubleValue" value-obj))
    ((gethash "timestampValue" value-obj) (gethash "timestampValue" value-obj))
    (t nil)))

(defun doc-to-product (doc)
  (let* ((fields (gethash "fields" doc))
         (full-name (gethash "name" doc))
         ;; Extract ID from full path: projects/.../documents/inventory/ID
         (id (car (last (uiop:split-string full-name :separator "/")))))
    (make-product
     :id id
     :name (parse-firestore-value (gethash "name" fields))
     :price (parse-firestore-value (gethash "price" fields))
     :quantity (parse-firestore-value (gethash "quantity" fields))
     :imgfile (parse-firestore-value (gethash "imgfile" fields))
     :timestamp (parse-firestore-value (gethash "timestamp" fields))
     :actualdateadded (parse-firestore-value (gethash "actualdateadded" fields)))))

;;; Core Operations

(defun init-firestore ()
  (unless (get-project-id)
    (log-info "GOOGLE_CLOUD_PROJECT not set. Firestore disabled.")
    (return-from init-firestore nil))
  
  (handler-case
      (let ((url (format nil "~A/inventory?pageSize=1" (firestore-base-url))))
        (firestore-request :get url)
        (setf *db-running* t)
        (log-info "Firestore connected successfully.")
        t)
    (error (e)
      (log-info (format nil "Error connecting to Firestore: ~A" e))
      (setf *db-running* nil)
      nil)))

(defun get-products ()
  (unless *db-running* (error "Database not running"))
  (let* ((url (format nil "~A/inventory" (firestore-base-url)))
         (response (firestore-request :get url))
         (json (yason:parse response))
         (documents (gethash "documents" json)))
    (mapcar #'doc-to-product documents)))

(defun get-product-by-id (id)
  (unless *db-running* (error "Database not running"))
  (let ((url (format nil "~A/inventory/~A" (firestore-base-url) id)))
    (handler-case
        (let ((response (firestore-request :get url)))
          (doc-to-product (yason:parse response)))
      (dex:http-request-not-found () nil))))

(defun search-products (query)
  (let ((products (get-products)))
    (remove-if-not (lambda (p) 
                     (search query (product-name p) :test #'char-equal))
                   products)))

;;; Seeding Helper

(defun create-firestore-doc-body (p)
  (let ((fields (make-hash-table :test 'equal)))
    (setf (gethash "name" fields) (alexandria:plist-hash-table (list "stringValue" (product-name p))))
    (setf (gethash "price" fields) (alexandria:plist-hash-table (list "integerValue" (princ-to-string (product-price p)))))
    (setf (gethash "quantity" fields) (alexandria:plist-hash-table (list "integerValue" (princ-to-string (product-quantity p)))))
    (setf (gethash "imgfile" fields) (alexandria:plist-hash-table (list "stringValue" (product-imgfile p))))
    (setf (gethash "timestamp" fields) (alexandria:plist-hash-table (list "timestampValue" (product-timestamp p))))
    (setf (gethash "actualdateadded" fields) (alexandria:plist-hash-table (list "timestampValue" (product-actualdateadded p))))
    (alexandria:plist-hash-table (list "fields" fields))))

(defun run-query-by-name (name)
  (let ((url (format nil "~A:runQuery" (firestore-base-url)))
        (query (alexandria:plist-hash-table
                (list "structuredQuery"
                      (alexandria:plist-hash-table
                       (list "from" (list (alexandria:plist-hash-table (list "collectionId" "inventory")))
                             "where" (alexandria:plist-hash-table
                                      (list "fieldFilter"
                                            (alexandria:plist-hash-table
                                             (list "field" (alexandria:plist-hash-table (list "fieldPath" "name"))
                                                   "op" "EQUAL"
                                                   "value" (alexandria:plist-hash-table (list "stringValue" name))))))))))))
    (let ((response (firestore-request :post url query)))
      ;; Response is a list of objects, one of which might contain "document"
      (let ((results (yason:parse response)))
        ;; results is a list. filter for entries with "document"
        (loop for entry in results
              when (gethash "document" entry)
              collect (gethash "document" entry))))))

(defun add-or-update-product (p)
  ;; Check if exists by name
  (let ((existing-docs (run-query-by-name (product-name p))))
    (if existing-docs
        ;; Update
        (dolist (doc existing-docs)
          (let* ((doc-name (gethash "name" doc)) ;; full path
                 (url (format nil "https://firestore.googleapis.com/v1/~A" doc-name)))
            ;; Using PATCH
            (firestore-request :patch url (create-firestore-doc-body p))))
        ;; Create
        (let ((url (format nil "~A/inventory" (firestore-base-url))))
          (firestore-request :post url (create-firestore-doc-body p))))))

(defun random-date-iso (seconds-ago)
  (local-time:format-timestring nil 
                                (local-time:timestamp- (local-time:now) (random seconds-ago) :sec)
                                :format '(:year "-" :month "-" :day "T"
                                          :hour ":" :min ":" :sec "Z")))

(defun make-seed-product (name &key (max-price 10) (max-quantity 500) (age-seconds 31536000))
  (make-product
   :name name
   :price (+ 1 (random max-price))
   :quantity (random max-quantity)
   :imgfile (format nil "product-images/~A.png" (string-downcase (remove #\Space name)))
   :timestamp (random-date-iso age-seconds)
   :actualdateadded (local-time:format-timestring nil (local-time:now) :format '(:year "-" :month "-" :day "T" :hour ":" :min ":" :sec "Z"))))

(defun seed-database ()
  (unless *db-running* (error "Database not running"))
  (let ((names '("Apples" "Bananas" "Milk" "Whole Wheat Bread" "Eggs" "Cheddar Cheese"
                 "Whole Chicken" "Rice" "Black Beans" "Bottled Water" "Apple Juice"
                 "Cola" "Coffee Beans" "Green Tea" "Watermelon" "Broccoli"
                 "Jasmine Rice" "Yogurt" "Beef" "Shrimp" "Walnuts"
                 "Sunflower Seeds" "Fresh Basil" "Cinnamon")))
    (dolist (name names)
      (log-info (format nil "Adding/Updating ~A" name))
      (add-or-update-product (make-seed-product name))))
  
  (let ((recent '("Parmesan Crisps" "Pineapple Kombucha" "Maple Almond Butter"
                  "Mint Chocolate Cookies" "White Chocolate Caramel Corn" "Acai Smoothie Packs"
                  "Smores Cereal" "Peanut Butter and Jelly Cups")))
    (dolist (name recent)
      (log-info (format nil "Adding/Updating ~A" name))
      (add-or-update-product (make-seed-product name :age-seconds 518400))))
      
   (let ((oos '("Wasabi Party Mix" "Jalapeno Seasoning")))
    (dolist (name oos)
      (log-info (format nil "Adding/Updating OOS ~A" name))
      (let ((p (make-seed-product name :age-seconds 518400)))
        (setf (product-quantity p) 0)
        (add-or-update-product p)))))

(defun reset-database ()
  (unless *db-running* (error "Database not running"))
  (let ((products (get-products)))
    (dolist (p products)
      (let ((url (format nil "~A/inventory/~A" (firestore-base-url) (product-id p))))
        (firestore-request :delete url)))))
