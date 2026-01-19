(defpackage :firestore-stdio-lisp.inventory
  (:use :cl)
  (:import-from :firestore-stdio-lisp.firestore
                :init-firestore
                :get-collection
                :get-document
                :create-document
                :delete-document
                :*firestore-project-id*)
  (:import-from :firestore-stdio-lisp.logger :log-info)
  (:import-from :local-time)
  (:import-from :alexandria)
  (:export :ensure-db-running
           :seed-database
           :reset-database
           :get-all-products
           :get-product-by-id
           :db-running-p))

(in-package :firestore-stdio-lisp.inventory)

(defvar *db-running* nil)

(defun db-running-p ()
  *db-running*)

(defun ensure-db-running ()
  (unless *db-running*
    (setf *db-running* (init-firestore))
    (if *db-running*
        (log-info "Firestore initialized successfully.")
        (log-info "Failed to initialize Firestore.")))
  *db-running*)

(defun make-product (&key name price quantity imgfile timestamp actualdateadded)
  (let ((h (make-hash-table :test 'equal)))
    (setf (gethash "name" h) name)
    (setf (gethash "price" h) price)
    (setf (gethash "quantity" h) quantity)
    (setf (gethash "imgfile" h) imgfile)
    (setf (gethash "timestamp" h) timestamp)
    (setf (gethash "actualdateadded" h) actualdateadded)
    h))

(defun random-days-ago (min-days max-days)
  (let ((now (local-time:now))
        (seconds-to-subtract (+ (* min-days 24 60 60)
                                (random (* (- max-days min-days) 24 60 60)))))
    (local-time:timestamp- (local-time:now) seconds-to-subtract :sec)))

(defun seed-database ()
  (unless (ensure-db-running)
    (error "Database not running"))
  
  (log-info "Seeding database...")
  
  (let ((old-products '("Apples" "Bananas" "Milk" "Whole Wheat Bread" "Eggs" "Cheddar Cheese"
                        "Whole Chicken" "Rice" "Black Beans" "Bottled Water" "Apple Juice"
                        "Cola" "Coffee Beans" "Green Tea" "Watermelon" "Broccoli"
                        "Jasmine Rice" "Yogurt" "Beef" "Shrimp" "Walnuts"
                        "Sunflower Seeds" "Fresh Basil" "Cinnamon"))
        (recent-products '("Parmesan Crisps" "Pineapple Kombucha" "Maple Almond Butter"
                           "Mint Chocolate Cookies" "White Chocolate Caramel Corn" "Acai Smoothie Packs"
                           "Smores Cereal" "Peanut Butter and Jelly Cups"))
        (oos-products '("Wasabi Party Mix" "Jalapeno Seasoning")))

    ;; Add Old Products
    (dolist (name old-products)
      (let* ((price (+ 1 (random 10)))
             (quantity (+ 1 (random 500)))
             (imgfile (format nil "product-images/~A.png" (string-downcase (remove #\Space name))))
             ;; Approx logic from TS: 31536000000ms = ~1 year, 7776000000ms = ~90 days
             ;; So between 90 days and 1 year+90 days ago
             (timestamp (random-days-ago 90 455)) 
             (product (make-product :name name
                                    :price price
                                    :quantity quantity
                                    :imgfile imgfile
                                    :timestamp timestamp
                                    :actualdateadded (local-time:now))))
        (log-info (format nil "Adding old product: ~A" name))
        ;; We use name as ID for simplicity or just let Firestore generate one?
        ;; The TS code queries by name to update, or adds.
        ;; Here we will just create new ones to simplify or query first?
        ;; For simplicity in this port, we'll try to use a deterministic ID based on name to simulate "update or add"
        ;; effectively overwriting.
        (create-document "inventory" product (string-downcase (remove #\Space name)))))

    ;; Add Recent Products
    (dolist (name recent-products)
      (let* ((price (+ 1 (random 10)))
             (quantity (+ 1 (random 100)))
             (imgfile (format nil "product-images/~A.png" (string-downcase (remove #\Space name))))
             ;; < 6 days ago (518400000ms)
             (timestamp (random-days-ago 0 6))
             (product (make-product :name name
                                    :price price
                                    :quantity quantity
                                    :imgfile imgfile
                                    :timestamp timestamp
                                    :actualdateadded (local-time:now))))
        (log-info (format nil "Adding recent product: ~A" name))
        (create-document "inventory" product (string-downcase (remove #\Space name)))))

    ;; Add OOS Products
    (dolist (name oos-products)
      (let* ((price (+ 1 (random 10)))
             (quantity 0)
             (imgfile (format nil "product-images/~A.png" (string-downcase (remove #\Space name))))
             (timestamp (random-days-ago 0 6))
             (product (make-product :name name
                                    :price price
                                    :quantity quantity
                                    :imgfile imgfile
                                    :timestamp timestamp
                                    :actualdateadded (local-time:now))))
        (log-info (format nil "Adding OOS product: ~A" name))
        (create-document "inventory" product (string-downcase (remove #\Space name))))))
  
  (log-info "Database seeded."))

(defun reset-database ()
  (unless (ensure-db-running)
    (error "Database not running"))
  (log-info "Resetting database...")
  (let ((products (get-collection "inventory")))
    (dolist (p products)
      (let ((id (gethash "id" p)))
        (when id
          (delete-document "inventory" id)))))
  (log-info "Database reset."))

(defun get-all-products ()
  (unless (ensure-db-running)
    (error "Database not running"))
  (get-collection "inventory"))

(defun get-product-by-id (id)
  (unless (ensure-db-running)
    (error "Database not running"))
  (get-document "inventory" id))
