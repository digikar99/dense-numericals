(in-package :dense-numericals.impl)


(cffi:load-foreign-library (cl:merge-pathnames #P"../c-src/libdense-numericals.so"
                                               *src-dir*))

(5am:def-suite* array :in :dense-numericals)

;; Basic Concept:
;; (c:dn-ssin (array-total-size x) (ptr x) 1 (ptr out) 1)

(defparameter dn:*multithreaded-threshold* 80000)
(declaim (type fixnum dn:*multithreaded-threshold*))

(defmacro define-one-arg-functions (name single-float-c-name double-float-c-name)

  ;; TODO: Use ARRAY or STATIC-ARRAY
  `(progn

     ;; TODO: Incorporate multi-threading for strided arrays
     (defpolymorph (,name :inline t)
         ((x (array single-float)) &key ((out (array single-float))
                                         (zeros-like x)))
         (array single-float)
       (ptr-iterate-but-inner n ((ptr-x   4 ix   x)
                                 (ptr-out 4 iout out))
                              (,single-float-c-name n ptr-x ix ptr-out iout))
       out)

     ;; There isn't much benefit to SIMPLE-ARRAYs even for 2 dimensional arrays
     ;; But the benefit keeps increasing with increasing number of dimensions
     (defpolymorph (,name :inline t)
         ((x (simple-array single-float)) &key ((out (simple-array single-float))
                                                (zeros-like x)))
         (simple-array single-float)
       (let ((total-size (array-total-size x)))
         (if (< total-size dn:*multithreaded-threshold*)
             (,single-float-c-name total-size (ptr x) 1 (ptr out) 1)
             (let* ((worker-count  (lparallel:kernel-worker-count))
                    (max-work-size (ceiling total-size worker-count)))
               (declare (type dense-arrays::size max-work-size worker-count))
               (lparallel:pdotimes (thread-idx worker-count)
                 (declare (type dense-arrays::size thread-idx))
                 (,single-float-c-name (min max-work-size
                                            (the-size
                                             (- total-size (the-size
                                                            (* max-work-size
                                                               thread-idx)))))
                                       (cffi:inc-pointer (ptr x)
                                                         (the-size
                                                          (* 4 thread-idx max-work-size)))
                                       1
                                       (cffi:inc-pointer (ptr out)
                                                         (the-size
                                                          (* 4 thread-idx max-work-size)))
                                       1)))))
       out)

     (defpolymorph (,name :inline t)
         ((x (array double-float)) &key ((out (array double-float))
                                         (zeros-like x)))
         (array double-float)
       (ptr-iterate-but-inner n ((ptr-x   8 ix   x)
                                 (ptr-out 8 iout out))
                              (,double-float-c-name n ptr-x ix ptr-out iout))
       out)

     (defpolymorph (,name :inline t)
         ((x (simple-array double-float)) &key ((out (simple-array double-float))
                                                (zeros-like x)))
         (simple-array double-float)
       (let ((total-size (array-total-size x)))
         (if (< total-size dn:*multithreaded-threshold*)
             (,double-float-c-name total-size (ptr x) 1 (ptr out) 1)
             (let* ((worker-count  (lparallel:kernel-worker-count))
                    (max-work-size (ceiling total-size worker-count)))
               (declare (type dense-arrays::size max-work-size worker-count))
               (lparallel:pdotimes (thread-idx worker-count)
                 (declare (type dense-arrays::size thread-idx))
                 (,double-float-c-name (min max-work-size
                                            (the-size
                                             (- total-size (the-size
                                                            (* max-work-size
                                                               thread-idx)))))
                                       (cffi:inc-pointer (ptr x)
                                                         (the-size
                                                          (* 8
                                                             (the-size
                                                              (* thread-idx max-work-size)))))
                                       1
                                       (cffi:inc-pointer (ptr out)
                                                         (the-size
                                                          (* 8
                                                             (the-size
                                                              (* thread-idx max-work-size)))))
                                       1)))))
       out)

     ;; TODO: Implement these for complex-floats

     ;; It's SBCL who does not emit compiler notes :/
     (defpolymorph (,name :inline t) ((x number) &key ((out null) nil outp))
         number
       (declare (ignorable out outp))
       (,(find-symbol (symbol-name name) :cl) x))

     ;; TODO: Implement a compiler-macro function for this
     ;; TODO: Rethink over default floating value
     (defpolymorph (,name :inline t) ((x list) &key ((out null) nil outp))
         (values array &optional)
       (declare (ignorable out outp))
       (,name (asarray x)))))


(macrolet ((def (name
                 (single-float-c-name single-float-error
                  &optional (sf-min 0.0f0) (sf-max 1.0f0))
                 (double-float-c-name double-float-error
                  &optional (df-min 0.0d0) (df-max 1.0d0)))
             (eval `(define-polymorphic-function ,name (x &key out) :overwrite t))
             `(progn
                (define-polymorphic-function ,name (x &key out))
                (define-one-arg-functions ,name ,single-float-c-name ,double-float-c-name)
                ;; If someone is worried about the compilation time; then know that that comes
                ;; from this def-test form :/
                (define-numericals-one-arg-test ,name array
                    (,single-float-error ,sf-min ,sf-max)
                    (,double-float-error ,df-min ,df-max)))))
  (def dn:sin (c:dn-ssin 2f-7) (c:dn-dsin 1d-15))
  (def dn:cos (c:dn-scos 2f-7) (c:dn-dcos 1d-15))
  (def dn:tan (c:dn-stan 2f-7) (c:dn-dtan 1d-15))

  (def dn:asin (c:dn-sasin 2f-7) (c:dn-dasin 1d-15))
  (def dn:acos (c:dn-sacos 2f-7) (c:dn-dacos 1d-15))
  ;; (def dn:atan (c:dn-satan 2f-7) (c:dn-datan 1d-15)) ; Handle atan case specially

  (def dn:sinh (c:dn-ssinh 2f-7) (c:dn-dsinh 1d-15))
  (def dn:cosh (c:dn-scosh 2f-7) (c:dn-dcosh 1d-15))
  (def dn:tanh (c:dn-stanh 2f-7) (c:dn-dtanh 1d-15))

  (def dn:asinh (c:dn-sasinh 2f-7) (c:dn-dasinh 1d-15))
  (def dn:acosh (c:dn-sacosh 2f-7 1.0f0 2.0f0) (c:dn-dacosh 1d-15 1.0d0 2.0d0))
  (def dn:atanh (c:dn-satanh 2f-7) (c:dn-datanh 1d-15))

  (def dn:exp (c:dn-sexp 2f-7) (c:dn-dexp 1d-15))
  (def dn:sqrt (c:dn-ssqrt 2f-7) (c:dn-dsqrt 1d-15))
  (def dn:abs (c:dn-sfabs 0.0f0) (c:dn-dfabs 0.0f0)))

;; Handle atan case specially
(define-polymorphic-function dn:atan (x &rest args) :overwrite t)
(define-one-arg-functions dn:atan c:dn-satan c:dn-datan)
(define-numericals-one-arg-test dn:atan array (2f-7) (1d-15))


(macrolet ((def (name
                 (single-float-c-name single-float-error)
                 (double-float-c-name double-float-error))
             (eval `(define-polymorphic-function ,name (value &rest args) :overwrite t))
             `(progn
                (define-polymorphic-function ,name (value &rest args))
                (define-one-arg-functions ,name ,single-float-c-name ,double-float-c-name)
                (define-numericals-one-arg-test ,name array
                    (,single-float-error) (,double-float-error)))))
  (def dn:log (c:dn-slog 2f-7) (c:dn-dlog 1d-15))
  (def dn:fround (c:dn-sround 0.0f0) (c:dn-dround 0.0d0))
  (def dn:ftruncate (c:dn-strunc 0.0f0) (c:dn-dtrunc 0.0d0))
  (def dn:ffloor (c:dn-sfloor 0.0f0) (c:dn-dfloor 0.0d0))
  (def dn:fceiling (c:dn-sceil 0.0f0) (c:dn-dceil 0.0d0)))

(macrolet ((def (name op)
             `(progn
                (define-polymorphic-function ,name (x &key out) :overwrite t)
                (defpolymorph ,name ((x number) &key ((out null))) number
                  (declare (ignore out))
                  (,op x)))))
  (def dn:one-arg-- cl:-)
  (def dn:one-arg-/ cl:/))
