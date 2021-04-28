(in-package :dense-numericals.impl)


(cffi:load-foreign-library (cl:merge-pathnames #P"../c-src/libdense-numericals.so"
                                               *src-dir*))

(5am:def-suite* array :in :dense-numericals)

;; Basic Concept:
;; (c:dn-ssin (array-total-size x) (ptr x) 1 (ptr out) 1)

(defmacro define-one-arg-functions (name single-float-c-name double-float-c-name)

  ;; TODO: Use ARRAY or STATIC-ARRAY
  `(progn

     (defpolymorph (,name :inline t)
         ((x (array single-float)) &key ((out (array single-float))
                                         (zeros-like x)))
         (values (array single-float) &optional)
       (ptr-iterate-but-inner 4 n ((ptr-x   ix   x)
                                   (ptr-out iout out))
                              (,single-float-c-name n ptr-x ix ptr-out iout))
       out)

     ;; There isn't much benefit to SIMPLE-ARRAYs even for 2 dimensional arrays
     ;; But the benefit keeps increasing with increasing number of dimensions
     (defpolymorph (,name :inline t)
         ((x (simple-array single-float)) &key ((out (simple-array single-float))
                                                (zeros-like x)))
         (values (simple-array single-float) &optional)
       (let ((ptr-x   (ptr x))
             (ptr-out (ptr out))
             (n       (array-total-size x)))
         (,single-float-c-name n
                               ptr-x   1
                               ptr-out 1))
       out)

     (defpolymorph (,name :inline t)
         ((x (array double-float)) &key ((out (array double-float))
                                         (zeros-like x)))
         (values (array double-float) &optional)
       (ptr-iterate-but-inner 8 n ((ptr-x   ix   x)
                                   (ptr-out iout out))
                              (,double-float-c-name n ptr-x ix ptr-out iout))
       out)

     (defpolymorph (,name :inline t)
         ((x (simple-array double-float)) &key ((out (simple-array double-float))
                                                (zeros-like x)))
         (values (simple-array double-float) &optional)
       (let ((ptr-x   (ptr x))
             (ptr-out (ptr out))
             (n       (array-total-size x)))
         (,double-float-c-name n
                               ptr-x   1
                               ptr-out 1))
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
  (def dn:sqrt (c:dn-ssqrt 2f-7) (c:dn-dsqrt 2f-7)))

;; Handle atan case specially
(define-polymorphic-function dn:atan (x &rest args) :overwrite t)
(define-one-arg-functions dn:atan c:dn-satan c:dn-datan)
(define-numericals-one-arg-test dn:atan array (2f-7) (1d-15))


(macrolet ((def (name op)
             `(progn
                (define-polymorphic-function ,name (x &key out) :overwrite t)
                (defpolymorph ,name ((x number) &key ((out null))) number
                  (declare (ignore out))
                  (,op x)))))
  (def dn:one-arg-- cl:-)
  (def dn:one-arg-/ cl:/))
