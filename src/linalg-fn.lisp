(in-package :dense-numericals.impl)
(5am:in-suite :dense-numericals)

(cffi:load-foreign-library #p"/usr/lib/x86_64-linux-gnu/libblas.so")

;; Basic Concept:
;; (c:dn-ssin (array-total-size x) (ptr x) 1 (ptr out) 1)
