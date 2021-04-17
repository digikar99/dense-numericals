(asdf:defsystem "dense-numericals"
  :pathname "src/"
  :version "0.1.0"
  :serial t
  :depends-on ("dense-arrays-plus-lite"
               "cl-autowrap"
               "alexandria"
               "iterate"
               "uiop"
               "cffi"
               "fiveam"
               "adhoc-polymorphic-functions"
               "dense-arrays+static-vectors"
               "trivial-package-local-nicknames")
  :components ((:file "package")
               (:file "spec")
               (:file "linalg")
               (:file "ptr-iterate-but-inner")
               (:file "test")
               (:file "one-arg-fn")))

