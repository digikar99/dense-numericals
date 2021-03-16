(in-package :dense-numericals.c)

(autowrap:c-include (cl:merge-pathnames #P"../c-src/dense-numericals.h" *src-dir*)
                    :spec-path (cl:merge-pathnames #P"specs/" *src-dir*))
