(in-package :dense-numericals.impl)
(5am:in-suite :dense-numericals)

;; Basic Concept:
;; (c:dn-ssin (array-total-size x) (ptr x) 1 (ptr out) 1)

(macrolet ((def (name cl-name)
             `(progn
                (define-polymorphic-function ,name (x y &key out) :overwrite t)
                (defpolymorph ,name ((x number) (y number)
                                     &key ((out null)))
                    number
                  (declare (ignore out))
                  (,cl-name x y)))))
  
  (def dn:two-arg-+ +)
  (def dn:two-arg-- -)
  (def dn:two-arg-/ /)
  (def dn:two-arg-* *))


(macrolet ((def (name cl-name)
             `(progn
                (define-polymorphic-function ,name (x y &key out) :overwrite t)
                (defpolymorph ,name ((x number) (y number)
                                     &key ((out null)))
                    bit
                  (declare (ignore out))
                  ;; FIXME: What behavior do we want?
                  (if (,cl-name x y) 1 0)))))
  (def dn:two-arg-<  <)
  (def dn:two-arg-<= <=)
  (def dn:two-arg-=  =)
  (def dn:two-arg-/= /=)
  (def dn:two-arg->  >)
  (def dn:two-arg->= >=))



(defmacro define-two-arg-functions (name
                                    (first-arg second-arg)
                                    single-float-c-name
                                    double-float-c-name)
    `(progn
       
       (defpolymorph ,name ((,first-arg number) (,second-arg number) &key ((out null) nil))
           number
         (declare (ignore out))
         (,(find-symbol (symbol-name name) :cl) ,first-arg ,second-arg))
       
       (defpolymorph ,name ((,first-arg (array single-float)) (,second-arg (array single-float))
                            &key ((out (array single-float)) (zeros-like ,first-arg)))
           (array single-float)
         ;; TODO: Handle broadcasting
         ;; TODO: Handle cases reducible to exp2, exp and exp10
         (ptr-iterate-but-inner 4 n ((ptr-x ix ,first-arg)
                                     (ptr-y iy ,second-arg)
                                     (ptr-o io out))
                                (,single-float-c-name n
                                                      ptr-x ix
                                                      ptr-y iy
                                                      ptr-o io))
         out)

       (defpolymorph ,name ((,first-arg (simple-array single-float))
                            (,second-arg (simple-array single-float))
                            &key ((out (simple-array single-float)) (zeros-like ,first-arg)))
           (simple-array single-float)
         ;; TODO: Handle cases reducible to exp2, exp and exp10
         (let ((ptr-x (ptr ,first-arg))
               (ptr-y (ptr ,second-arg))
               (ptr-o (ptr out))
               (n     (array-total-size ,first-arg)))
         (,single-float-c-name n
                               ptr-x 1
                               ptr-y 1
                               ptr-o 1))
         out)

       (defpolymorph ,name ((,first-arg (array double-float)) (,second-arg (array double-float))
                            &key ((out (array double-float)) (zeros-like ,first-arg)))
           (array double-float)
         ;; TODO: Handle broadcasting
         ;; TODO: Handle cases reducible to exp2, exp and exp10
         (ptr-iterate-but-inner 8 n ((ptr-x ix ,first-arg)
                                     (ptr-y iy ,second-arg)
                                     (ptr-o io out))
                                (,double-float-c-name n
                                                      ptr-x ix
                                                      ptr-y iy
                                                      ptr-o io))
         out)

       (defpolymorph ,name ((,first-arg (simple-array double-float))
                            (,second-arg (simple-array double-float))
                            &key ((out (simple-array double-float)) (zeros-like ,first-arg)))
           (simple-array double-float)
         ;; TODO: Handle cases reducible to exp2, exp and exp10
         (let ((ptr-x (ptr ,first-arg))
               (ptr-y (ptr ,second-arg))
               (ptr-o (ptr out))
               (n     (array-total-size ,first-arg)))
           (,double-float-c-name n
                                 ptr-x 1
                                 ptr-y 1
                                 ptr-o 1))
         out)
       
       (defpolymorph (,name :inline t)
           ((,first-arg list) (,second-arg list) &key ((out null) nil))
           (values array &optional)
         (declare (ignorable out))
         (,name (asarray ,first-arg) (asarray ,second-arg)))))


(macrolet ((def (name (first-arg second-arg)
                 (single-float-c-name single-float-error
                  &optional (sf-min 0.0f0) (sf-max 1.0f0))
                 (double-float-c-name double-float-error
                  &optional (df-min 0.0d0) (df-max 1.0d0)))
             (eval `(define-polymorphic-function ,name (,first-arg ,second-arg &key out)
                      :overwrite t))
             `(progn
                (define-polymorphic-function ,name (,first-arg ,second-arg &key out))
                (define-two-arg-functions ,name (,first-arg ,second-arg)
                                          ,single-float-c-name ,double-float-c-name)
                ;; If someone is worried about the compilation time; then know that that comes
                ;; from this def-test form :/
                (define-numericals-two-arg-test ,name array
                    (,single-float-error ,sf-min ,sf-max single-float)
                    (,double-float-error ,df-min ,df-max double-float)))))

  (def dn:expt (base power)
    (c:dn-spow 2f-7)
    (c:dn-dpow 1d-15)))

(define-two-arg-functions dn:atan (x y) c:dn-satan2 c:dn-datan2)
;; (define-numericals-two-arg-test dn:atan array
;;     (2f-7  0.0f0 1.0f0 single-float)
;;     (1d-15 0.0d0 1.0d0 double-float))
