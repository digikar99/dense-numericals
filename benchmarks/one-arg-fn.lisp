(in-package :dense-numericals/benchmarks)

(in-suite :dense-numericals/benchmarks)

;; Credits: https://github.com/digikar99/numericals
(eval-when (:compile-toplevel :load-toplevel :execute)
  (pyexec "
def numpy_one_arg_fn(fn, a_sizes, o_sizes, num_operations, elt_type):
  import time
  import numpy as np

  timings = []

  for i in range(len(num_operations)):
    a = np.ones(a_sizes[i]).astype(elt_type)
    o = np.ones(o_sizes[i]).astype(elt_type)
    num_operation = int(num_operations[i] / (np.product(o_sizes[i])))
    start = time.time()
    for i in range(num_operation):
      fn(a, out = o)
    end = time.time()
    timings.append(end - start)

  return tuple(timings)
")

  (pyexec "
def torch_one_arg_fn(fn, a_sizes, o_sizes, num_operations, elt_type):
  import time
  import math as m
  import torch as t

  timings = []

  for i in range(len(num_operations)):
    a = t.ones(a_sizes[i], dtype=elt_type)
    o = t.ones(o_sizes[i], dtype=elt_type)
    num_operation = int(num_operations[i] / (m.prod(o_sizes[i])))
    start = time.time()
    for i in range(num_operation):
      fn(a, out = o)
    end = time.time()
    timings.append(end - start)

  return tuple(timings)
"))

(defpyfun "numpy_one_arg_fn")
(defpyfun "torch_one_arg_fn")

;; This is better suited as a function because we don't want to care about the
;; "form" of the arguments - we want to play with the values of the arguments.
(defun dn-one-arg-fn (&key fn a-sizes o-sizes num-operations elt-type)
  (loop :for i :below (length num-operations)
        :for a-size := (elt a-sizes i)
        :for o-size := (elt o-sizes i)
        :collect
        (let ((a (ones a-size :type elt-type))
              (o (ones o-size :type elt-type))
              (num-operation (floor (/ (elt num-operations i)
                                       (apply #'* o-size))))
              (fn (compile nil `(lambda (a &key out)
                                  (declare (optimize speed)
                                           (type (simple-array ,elt-type) a out))
                                  (funcall ',fn a :out out)))))
          (time-it
            (loop :for i :of-type fixnum :below num-operation
                  :do (funcall fn a :out o))))))

(defun one-arg-fn (lisp-names numpy-names &optional torch-names)
  (pyexec "import numpy as np")
  (pyexec "import torch as t")
  (let* ((a-sizes '((10 1) (10 10) (100 100) (1000 1000) (10000 10000)))
         (o-sizes '((10 1) (10 10) (100 100) (1000 1000) (10000 10000)))
         (num-operations '(1e7 1e8 1e9 1e9 1e9)))
    (loop :for idx :below (length lisp-names)
          :for numpy-name := (nth idx numpy-names)
          :for lisp-name  := (nth idx lisp-names)
          :for torch-name := (nth idx torch-names)
          :do (let ((dn-timings
                      (dn-one-arg-fn :fn lisp-name
                                     :a-sizes a-sizes
                                     :o-sizes o-sizes
                                     :num-operations num-operations
                                     :elt-type default-element-type))
                    (numpy-timings
                      (numpy-one-arg-fn :fn numpy-name
                                        :a-sizes a-sizes
                                        :o-sizes o-sizes
                                        :num-operations num-operations
                                        :elt-type (numpy-element-type default-element-type)))
                    (torch-timings
                      (when torch-name
                        (torch-one-arg-fn :fn torch-name
                                          :a-sizes a-sizes
                                          :o-sizes o-sizes
                                          :num-operations num-operations
                                          :elt-type (torch-element-type default-element-type)))))
                (push (make-fun-report :name lisp-name
                                       :array-sizes o-sizes
                                       :numpy numpy-timings
                                       :lisp dn-timings
                                       :torch torch-timings)
                      (report-fun-reports *report*))))))
