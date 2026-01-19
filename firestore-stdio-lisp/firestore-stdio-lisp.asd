(asdf:defsystem "firestore-stdio-lisp"
  :description "A Common Lisp MCP stdio server."
  :version "1.0.0"
  :author "Gemini"
  :license "MIT"
  :depends-on (:40ants-mcp
               :serapeum
               :yason
               :local-time
               :alexandria
               :uiop
               :dexador
               :cl-ppcre)
  :pathname "src"
  :serial t
  :components ((:file "logger")
               (:file "packages")
               (:file "firestore")
               (:file "inventory")
               (:file "main"))
  :in-order-to ((asdf:test-op (asdf:test-op "firestore-stdio-lisp/tests")))
  :build-operation "program-op"
  :build-pathname "../firestore-stdio-lisp"
  :entry-point "firestore-stdio-lisp:main")

(asdf:defsystem "firestore-stdio-lisp/tests"
  :depends-on ("firestore-stdio-lisp"
               "rove")
  :components ((:module "tests"
                :components
                ((:file "packages")
                 (:file "main"))))
  :perform (asdf:test-op (o c) (uiop:symbol-call :rove :run c)))
