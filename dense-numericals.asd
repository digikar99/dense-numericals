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
               "trivial-package-local-nicknames")
  :components ((:file "package")
               (:file "spec")
               (:file "ptr-iterate-but-inner")
               (:file "one-arg-fn")))

