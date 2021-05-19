(in-package :dense-numericals.impl)

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
       (policy-cond:with-expectations (= safety 0)
           ((assertion (equalp (narray-dimensions x)
                               (narray-dimensions out))
                       (x out)
                       "Expected X and OUT to have same dimensions but they are~%  ~S  ~S"
                       (narray-dimensions x)
                       (narray-dimensions out)))
         (with-thresholded-multithreading (array-total-size out)
             (x out)
           (ptr-iterate-but-inner n ((ptr-x   4 ix   x)
                                     (ptr-out 4 iout out))
             (,single-float-c-name n ptr-x ix ptr-out iout))))
       out)

     ;; There isn't much benefit to SIMPLE-ARRAYs even for 2 dimensional arrays
     ;; But the benefit keeps increasing with increasing number of dimensions
     (defpolymorph (,name :inline t)
         ((x (simple-array single-float)) &key ((out (simple-array single-float))
                                                (zeros-like x)))
         (simple-array single-float)
       (policy-cond:with-expectations (= safety 0)
           ((assertion (equalp (narray-dimensions x)
                               (narray-dimensions out))
                       (x out)
                       "Expected X and OUT to have same dimensions but they are~%  ~S  ~S"
                       (narray-dimensions x)
                       (narray-dimensions out)))
         (with-thresholded-multithreading (array-total-size out)
             (:simple x out)
           (,single-float-c-name (array-total-size out)
                                 (ptr x 4)
                                 1
                                 (ptr out 4)
                                 1)))
       out)

     (defpolymorph (,name :inline t)
         ((x (array double-float)) &key ((out (array double-float))
                                         (zeros-like x)))
         (array double-float)
       (policy-cond:with-expectations (= safety 0)
           ((assertion (equalp (narray-dimensions x)
                               (narray-dimensions out))
                       (x out)
                       "Expected X and OUT to have same dimensions but they are~%  ~S  ~S"
                       (narray-dimensions x)
                       (narray-dimensions out)))
         (with-thresholded-multithreading (array-total-size out)
             (x out)
           (ptr-iterate-but-inner n ((ptr-x   8 ix   x)
                                     (ptr-out 8 iout out))
             (,double-float-c-name n ptr-x ix ptr-out iout))))
       out)

     (defpolymorph (,name :inline t)
         ((x (simple-array double-float)) &key ((out (simple-array double-float))
                                                (zeros-like x)))
         (simple-array double-float)
       (policy-cond:with-expectations (= safety 0)
           ((assertion (equalp (narray-dimensions x)
                               (narray-dimensions out))
                       (x out)
                       "Expected X and OUT to have same dimensions but they are~%  ~S  ~S"
                       (narray-dimensions x)
                       (narray-dimensions out)))
         (with-thresholded-multithreading (array-total-size out)
             (:simple x out)
           (,double-float-c-name (array-total-size out)
                                 (ptr x 8) 1
                                 (ptr out 8) 1)))
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
       (,name (asarray x)))

     ;; (defpolymorph (,name :inline t) ((x array) &key ((out array))) array
     ;;   (if (type= (array-element-type x)
     ;;              (array-element-type out))
     ;;       (,name x :out out)
     ;;       (,name ())))
     ))

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
  (def dn:sin (bmas:ssin 2f-7) (bmas:dsin 1d-15))
  (def dn:cos (bmas:scos 2f-7) (bmas:dcos 1d-15))
  (def dn:tan (bmas:stan 2f-7) (bmas:dtan 1d-15))

  (def dn:asin (bmas:sasin 2f-7) (bmas:dasin 1d-15))
  (def dn:acos (bmas:sacos 2f-7) (bmas:dacos 1d-15))
  ;; (def dn:atan (bmas:satan 2f-7) (bmas:datan 1d-15)) ; Handle atan case specially

  (def dn:sinh (bmas:ssinh 2f-7) (bmas:dsinh 1d-15))
  (def dn:cosh (bmas:scosh 2f-7) (bmas:dcosh 1d-15))
  (def dn:tanh (bmas:stanh 2f-7) (bmas:dtanh 1d-15))

  (def dn:asinh (bmas:sasinh 2f-7) (bmas:dasinh 1d-15))
  (def dn:acosh (bmas:sacosh 2f-7 1.0f0 2.0f0) (bmas:dacosh 1d-15 1.0d0 2.0d0))
  (def dn:atanh (bmas:satanh 2f-7) (bmas:datanh 1d-15))

  (def dn:exp (bmas:sexp 2f-7) (bmas:dexp 1d-15))
  ;; (def dn:sqrt (bmas:ssqrt 2f-7) (bmas:dsqrt 1d-15))
  (def dn:abs (bmas:sfabs 0.0f0) (bmas:dfabs 0.0f0)))

;; Handle atan case specially
(define-polymorphic-function dn:atan (x &rest args) :overwrite t)
(define-one-arg-functions dn:atan bmas:satan bmas:datan)
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
  (def dn:log (bmas:slog 2f-7) (bmas:dlog 1d-15))
  (def dn:fround (bmas:sround 0.0f0) (bmas:dround 0.0d0))
  (def dn:ftruncate (bmas:strunc 0.0f0) (bmas:dtrunc 0.0d0))
  (def dn:ffloor (bmas:sfloor 0.0f0) (bmas:dfloor 0.0d0))
  (def dn:fceiling (bmas:sceil 0.0f0) (bmas:dceil 0.0d0)))

(macrolet ((def (name op)
             `(progn
                (define-polymorphic-function ,name (x &key out) :overwrite t)
                (defpolymorph ,name ((x number) &key ((out null))) number
                  (declare (ignore out))
                  (,op x)))))
  (def dn:one-arg-- cl:-)
  (def dn:one-arg-/ cl:/))
