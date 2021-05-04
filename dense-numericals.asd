(asdf:defsystem "dense-numericals"
  :pathname "src/"
  :version "0.1.0"
  :depends-on ("dense-arrays-plus-lite"
               "cl-autowrap"
               "alexandria"
               "iterate"
               "uiop"
               "cffi"
               "fiveam"
               "lparallel"
               "policy-cond"
               "adhoc-polymorphic-functions"
               "dense-arrays+static-vectors"
               "trivial-package-local-nicknames")
  :components ((:file "spec")
               (:file "linalg")
               (:file "package"               :depends-on ("spec" "linalg"))
               (:file "ptr-iterate-but-inner" :depends-on ("package"))
               (:file "test"                  :depends-on ("package"))
               (:file "one-arg-fn"            :depends-on ("spec"
                                                           "test"
                                                           "ptr-iterate-but-inner"))
               (:file "two-arg-fn"            :depends-on ("spec"
                                                           "test"
                                                           "ptr-iterate-but-inner"))
               (:file "n-arg-fn"              :depends-on ("one-arg-fn"
                                                           "two-arg-fn"))
               (:file "blas"                  :depends-on ("linalg")))
  :perform (test-op (o c)
             (declare (ignore o c))
             ;; Or should we use STATIC?
             (eval (read-from-string "(LET* ((DENSE-ARRAYS:*DENSE-ARRAY-BACKEND* :CL))
                                        (5AM:RUN 'DENSE-NUMERICALS.IMPL::ARRAY))"))))

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

