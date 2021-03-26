(in-package :dense-numericals.impl)

;; TODO: Avoid hardcoding the path
(autowrap:c-include (cl:merge-pathnames #P"../c-src/dense-numericals.h" *src-dir*)
                    ;; TODO: Is there an "inline" option?
                    :spec-path (cl:merge-pathnames #P"specs/" *src-dir*)
                    :function-package :dense-numericals.c)

;; TODO: Delete the below code once declaim-inline option is added

(do-symbols (s (find-package :dense-numericals.c))
  (when (fboundp s)
    (proclaim `(inline ,s))))

(autowrap:c-include (merge-pathnames #P"../c-src/dense-numericals.h" *src-dir*)
                    :spec-path (merge-pathnames #P"specs/" *src-dir*)
                    :function-package :dense-numericals.c)




