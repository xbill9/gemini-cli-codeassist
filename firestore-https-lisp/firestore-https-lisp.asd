(asdf:defsystem "firestore-https-lisp"
  :description "A Common Lisp MCP https server."
  :version "1.0.0"
  :author "Gemini"
  :license "MIT"
  :depends-on (:40ants-mcp
               :serapeum
               :yason
               :local-time
               :alexandria
               :uiop
               :dexador)
  :pathname "src"
  :serial t
  :components ((:file "packages")
               (:file "logger")
               (:file "firestore")
               (:file "main"))
  :in-order-to ((asdf:test-op (asdf:test-op "firestore-https-lisp/tests")))
  :build-operation "program-op"
  :build-pathname "../mcp-server"
  :entry-point "mcp-server:main")

(asdf:defsystem "firestore-https-lisp/tests"
  :depends-on ("firestore-https-lisp"
               "rove")
  :components ((:module "tests"
                :components
                ((:file "packages")
                 (:file "main"))))
  :perform (asdf:test-op (o c) (uiop:symbol-call :rove :run c)))