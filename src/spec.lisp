(cl:in-package :cl)
(eval-when (:compile-toplevel :load-toplevel :execute)
  (unless (find-package :dense-numericals.c)
    (defpackage :dense-numericals.c
      (:use))))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defvar dense-numericals.c::*src-dir*
    (asdf:component-pathname (asdf:find-system "dense-numericals"))))

;; TODO: Avoid hardcoding the path
(autowrap:c-include (merge-pathnames #P"../c-src/dense-numericals.h"
                                     dense-numericals.c::*src-dir*)
                    ;; TODO: Is there an "inline" option?
                    :spec-path
                    (merge-pathnames #P"specs/" dense-numericals.c::*src-dir*)
                    :function-package :dense-numericals.c)

;; TODO: Delete the below code once declaim-inline option is added

(eval-when (:compile-toplevel :load-toplevel :execute)
  (do-symbols (s (find-package :dense-numericals.c))
    (when (fboundp s)
      (proclaim `(inline ,s)))))

(autowrap:c-include (cl:merge-pathnames #P"../c-src/dense-numericals.h"
                                        dense-numericals.c::*src-dir*)
                    :spec-path
                    (merge-pathnames #P"specs/" dense-numericals.c::*src-dir*)
                    :function-package :dense-numericals.c)




