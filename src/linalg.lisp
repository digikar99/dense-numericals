(cl:in-package :cl)
(eval-when (:compile-toplevel :load-toplevel :execute)
  (unless (find-package :dense-numericals.linalg.c)
    (defpackage :dense-numericals.linalg.c
      (:use))))


(cl:in-package :dense-numericals.linalg.c)
;; TODO: Avoid hardcoding the path
(autowrap:c-include #P"/usr/include/x86_64-linux-gnu/cblas.h"
                    :spec-path
                    (cl:merge-pathnames #P"linalg-specs/"
                                        (asdf:component-pathname (asdf:find-system "dense-numericals")))
                    :function-package :dense-numericals.linalg.c
                    :constant-package :dense-numericals.linalg.c
                    :release-p cl:t)

(cl:in-package :cl)
(eval-when (:compile-toplevel :load-toplevel :execute)
  (do-symbols (s (find-package :dense-numericals.linalg.c))
    (when (fboundp s)
      (proclaim `(inline ,s)))))


(cl:in-package :dense-numericals.linalg.c)
(autowrap:c-include #P"/usr/include/x86_64-linux-gnu/cblas.h"
                    :spec-path
                    (cl:merge-pathnames #P"linalg-specs/"
                                        (asdf:component-pathname (asdf:find-system "dense-numericals")))
                    :function-package :dense-numericals.linalg.c
                    :constant-package :dense-numericals.linalg.c
                    :release-p cl:t)
