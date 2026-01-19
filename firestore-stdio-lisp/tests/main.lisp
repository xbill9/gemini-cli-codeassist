(in-package :firestore-stdio-lisp-tests)

(deftest test-greeting
  (testing "should return a valid greeting"
    (ok (string= (get-greeting "World") "Hello, World!"))
    (ok (string= (get-greeting "Lisp") "Hello, Lisp!"))))