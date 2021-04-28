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
               (:file "linalg")
               (:file "ptr-iterate-but-inner")
               (:file "one-arg-fn")))

(asdf:defsystem "dense-numericals/benchmarks"
  :pathname "benchmarks/"
  :version "0.1.0"
  :depends-on ("alexandria"
               "cl-ascii-table"
               "dense-numericals"
               "py4cl2"
               "fiveam")
  :components ((:file "package")
               (:file "benchmark")
               (:file "one-arg-fn"))
  :perform (test-op (o c)
             (declare (ignore o c))
             ;; Or should we use STATIC?
             (eval (read-from-string "(LET* ((DENSE-ARRAYS:*DENSE-ARRAY-BACKEND* :CL))
                                        (5AM:RUN :DENSE-NUMERICALS/BENCHMARKS))"))))

