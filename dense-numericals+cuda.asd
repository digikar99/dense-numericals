(asdf:defsystem "dense-numericals+cuda"
  :pathname "cuda/"
  :version "0.1.0"
  :serial t
  :depends-on ("dense-numericals"
               "dense-arrays+cuda"
               "trivial-package-local-nicknames"
               "cl-cuda")
  :components ((:file "package")
               (:file "cuda")
               (:file "one-arg-fn")))
