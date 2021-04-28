(uiop:define-package :dense-numericals/benchmarks
  (:mix :dense-arrays-plus-lite :py4cl2 :alexandria :cl :fiveam)
  (:import-from :dense-arrays
                #:default-element-type)
  (:export #:benchmark
           #:report))

(in-package :dense-numericals/benchmarks)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (trivial-package-local-nicknames:add-package-local-nickname :dn :dense-numericals))

(def-suite* :dense-numericals/benchmarks)

