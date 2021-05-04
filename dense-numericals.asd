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
               "adhoc-polymorphic-functions"
               "dense-arrays+static-vectors"
               "trivial-package-local-nicknames")
  :components ((:file "package")
               (:file "spec"                  :depends-on ("package"))
               (:file "linalg"                :depends-on ("package"))
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
               (:file "two-arg-fn-adv"              :depends-on ("two-arg-fn")))
  :perform (test-op (o c)
             (declare (ignore o c))
             ;; Or should we use STATIC?
             (eval (read-from-string "(LET* ((DENSE-ARRAYS:*DENSE-ARRAY-BACKEND* :CL))
                                        (5AM:RUN 'DENSE-NUMERICALS.IMPL::ARRAY))"))))

