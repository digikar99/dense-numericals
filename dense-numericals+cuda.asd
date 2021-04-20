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
               (:file "one-arg-fn"))
  :perform (test-op (o c)
             (declare (ignore o c))
             (eval (read-from-string "(LET* ((DENSE-ARRAYS:*DENSE-ARRAY-BACKEND* :CUDA)
                                             (CL-CUDA:*SHOW-MESSAGES* NIL))
                                        (5AM:RUN 'DENSE-NUMERICALS.IMPL::CUDA-ARRAY))"))))
