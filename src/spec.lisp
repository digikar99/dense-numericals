(in-package :dense-numericals.c)

(cl:eval-when (:compile-toplevel :load-toplevel :execute)
  (autowrap:c-include (cl:merge-pathnames #P"../c-src/dense-numericals.h" *src-dir*)
                      :spec-path (cl:merge-pathnames #P"specs/" *src-dir*)))
