(cl:in-package :dense-numericals.impl)

;; TODO: Avoid hardcoding the path
(autowrap:c-include #P"/usr/include/x86_64-linux-gnu/cblas.h"
                    :spec-path (cl:merge-pathnames #P"linalg-specs/" *src-dir*)
                    :function-package :dense-numericals.linalg.c
                    :constant-package :dense-numericals.linalg.c)
