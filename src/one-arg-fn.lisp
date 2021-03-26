(in-package :dense-numericals.impl)
(5am:in-suite :dense-numericals)

(cffi:load-foreign-library (cl:merge-pathnames #P"../c-src/libdense-numericals.so"
                                               c:*src-dir*))

;; Basic Concept:
;; (c:dn-ssin (array-total-size x) (ptr x) 1 (ptr out) 1)

(defmacro define-one-arg-functions
    
    (name (single-float-c-name single-float-error)
          (double-float-c-name double-float-error))
  
  `(handler-bind ((style-warning #'muffle-warning))
     
     (define-polymorphic-function ,name (x &key out) :overwrite t)
     
     (defpolymorph (,name :recursively-safe-p t)
         ((x (array single-float)) &key ((out (array single-float))
                                         (zeros-like x)))
         (array single-float)
       (declare (optimize speed))
       (ptr-iterate-but-inner 4 n ((ptr-x   ix   x)
                                 (ptr-out iout out))
                              (,single-float-c-name n
                                                    ptr-x   ix
                                                    ptr-out iout))
       out)

     (defpolymorph (,name :recursively-safe-p t)
         ((x (array double-float)) &key ((out (array double-float))
                                         (zeros-like x)))
         (array double-float)
       (declare (optimize speed))
       (ptr-iterate-but-inner 8 n ((ptr-x   ix   x)
                                 (ptr-out iout out))
                              (,double-float-c-name n
                                                    ptr-x   ix
                                                    ptr-out iout))
       out)

     ;; TODO: Implement these for complex-floats

     ;; It's SBCL who does not emit compiler notes :/
     (defpolymorph (,name :recursively-safe-p t) ((x number) &key ((out null) nil outp))
         number
       (declare (ignorable out outp)
                (optimize speed))
       (,(find-symbol (symbol-name name) :cl) x))

     ;; TODO: Implement a compiler-macro function for this
     ;; TODO: Rethink over default floating value
     (defpolymorph (,name :recursively-safe-p t) ((x list) &key ((out null) nil outp)) array
       (declare (ignorable out outp)
                (optimize speed))
       (,name (the (array ,(cdr (assoc (find-package :dense-numericals.impl)
                                       *element-type-alist*)))
                   (asarray x))))

     (5am:def-test ,name ()
       (macrolet ((verify (type error)
                    `(progn
                       (5am:is-true (let ((rand (rand 1000 :type ,type)))
                                      (array= (macro-map-array 'cl:sin rand)
                                              (dn:sin rand)
                                              :test (lambda (x y)
                                                      (< (/ (abs (- x y)) (abs x))
                                                         ,error)))))
                       (5am:is-true (let* ((rand (aref (rand '(100 100) :type ,type)
                                                       '(10 :step 2))))
                                      (array= (macro-map-array 'cl:sin rand)
                                              (dn:sin rand :out rand)
                                              :test (lambda (x y)
                                                      (if (not (< (/ (abs (- x y)) (abs x))
                                                                  ,error))
                                                          (progn (print (list x y)) nil)
                                                          t)))))
                       (5am:is-true (let ((rand (aref (rand '(100 100) :type ,type)
                                                      '(10 :step 2)
                                                      '(10 :step 2))))
                                      (array= (macro-map-array 'cl:sin rand)
                                              (dn:sin rand :out rand)
                                              :test (lambda (x y)
                                                      (< (/ (abs (- x y)) (abs x))
                                                         ,error)))))
                       (5am:is-true (let ((rand (aref (rand '(100 100) :type ,type)
                                                      nil
                                                      '(10 :step -2))))
                                      (array= (macro-map-array 'cl:sin rand)
                                              (dn:sin rand :out rand)
                                              :test (lambda (x y)
                                                      (< (/ (abs (- x y)) (abs x))
                                                         ,error)))))
                       (5am:is-true (let* ((array (asarray '((1 2 3) (4 5 6))
                                                           ,type)))
                                      (dn:sin (aref array nil 1)
                                              :out (aref array nil 1))
                                      (equalp '(1 3 4 6)
                                              (list (aref array 0 0)
                                                    (aref array 0 2)
                                                    (aref array 1 0)
                                                    (aref array 1 2))))))))
         (verify 'single-float ,single-float-error)
         (verify 'double-float ,double-float-error)))))

(define-one-arg-functions dn:sin (c:dn-ssin 2f-7) (c:dn-dsin 1d-15))
(define-one-arg-functions dn:cos (c:dn-scos 2f-7) (c:dn-dcos 1d-15))
(define-one-arg-functions dn:tan (c:dn-stan 2f-7) (c:dn-dtan 1d-15))

(define-one-arg-functions dn:asin (c:dn-sasin 2f-7) (c:dn-dasin 1d-15))
(define-one-arg-functions dn:acos (c:dn-sacos 2f-7) (c:dn-dacos 1d-15))
(define-one-arg-functions dn:atan (c:dn-satan 2f-7) (c:dn-datan 1d-15))

(define-one-arg-functions dn:sinh (c:dn-ssinh 2f-7) (c:dn-dsinh 1d-15))
(define-one-arg-functions dn:cosh (c:dn-scosh 2f-7) (c:dn-dcosh 1d-15))
(define-one-arg-functions dn:tanh (c:dn-stanh 2f-7) (c:dn-dtanh 1d-15))

(define-one-arg-functions dn:asinh (c:dn-sasinh 2f-7) (c:dn-dasinh 1d-15))
(define-one-arg-functions dn:acosh (c:dn-sacosh 2f-7) (c:dn-dacosh 1d-15))
(define-one-arg-functions dn:atanh (c:dn-satanh 2f-7) (c:dn-datanh 1d-15))

(define-one-arg-functions dn:exp (c:dn-sexp 2f-7) (c:dn-dexp 1d-15))
(define-one-arg-functions dn:sqrt (c:dn-ssqrt 2f-7) (c:dn-dsqrt 2f-7))
