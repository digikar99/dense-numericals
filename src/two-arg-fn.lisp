(in-package :dense-numericals.impl)
(5am:in-suite :dense-numericals)

;; Basic Concept:
;; (c:dn-ssin (array-total-size x) (ptr x) 1 (ptr out) 1)

(declaim (inline broadcast-zeros))
(defun broadcast-zeros (a b element-type)
  ;; TODO: Raise error
  (zeros (nth-value 1 (broadcast-compatible-p a b))
         :type element-type))

(defmacro define-two-arg-functions
    (name cl-name
     (first-arg second-arg)
     (single-float-c-name single-float-return-type
      &optional (single-float-return-element-size 4))
     (double-float-c-name double-float-return-type
      &optional (double-float-return-element-size 8)))
  `(progn

     (defpolymorph ,name ((,first-arg number) (,second-arg number) &key ((out null) nil))
         number
       (declare (ignore out))
       ,(if (member cl-name '(cl:< cl:= cl:<= cl:/= cl:>= cl:>))
            `(if (,cl-name ,first-arg ,second-arg)
                 1
                 0)
            `(,cl-name ,first-arg ,second-arg)))

     (defpolymorph ,name ((,first-arg (array single-float)) (,second-arg (array single-float))
                          &key ((out (array ,single-float-return-type))
                                (broadcast-zeros ,first-arg ,second-arg
                                                 ',single-float-return-type)))
         (array ,single-float-return-type)
       ;; TODO: Handle broadcasting
       ;; TODO: Handle cases reducible to exp2, exp and exp10
       (ptr-iterate-but-inner n ((ptr-x 4                                 ix ,first-arg)
                                 (ptr-y 4                                 iy ,second-arg)
                                 (ptr-o ,single-float-return-element-size io out))
                              (,single-float-c-name n
                                                    ptr-x ix
                                                    ptr-y iy
                                                    ptr-o io))
       out)

     (defpolymorph ,name ((,first-arg (simple-array single-float))
                          (,second-arg (simple-array single-float))
                          &key ((out (simple-array ,single-float-return-type))
                                (broadcast-zeros ,first-arg ,second-arg
                                                 ',single-float-return-type)))
         (simple-array ,single-float-return-type)
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
                          &key ((out (array ,double-float-return-type))
                                (broadcast-zeros ,first-arg ,second-arg
                                                 ',double-float-return-type)))
         (array ,double-float-return-type)
       ;; TODO: Handle broadcasting
       ;; TODO: Handle cases reducible to exp2, exp and exp10
       (ptr-iterate-but-inner n ((ptr-x 8                                 ix ,first-arg)
                                 (ptr-y 8                                 iy ,second-arg)
                                 (ptr-o ,double-float-return-element-size io out))
                              (,double-float-c-name n
                                                    ptr-x ix
                                                    ptr-y iy
                                                    ptr-o io))
       out)

     (defpolymorph ,name ((,first-arg (simple-array double-float))
                          (,second-arg (simple-array double-float))
                          &key ((out (simple-array ,double-float-return-type))
                                (broadcast-zeros ,first-arg ,second-arg
                                                 ',double-float-return-type)))
         (simple-array ,double-float-return-type)
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


(macrolet ((def (name cl-name (first-arg second-arg)
                 (single-float-c-name single-float-error
                  &optional (sf-min 0.0f0) (sf-max 1.0f0))
                 (double-float-c-name double-float-error
                  &optional (df-min 0.0d0) (df-max 1.0d0)))
             (eval `(define-polymorphic-function ,name (,first-arg ,second-arg &key out)
                      :overwrite t))
             `(progn
                (define-polymorphic-function ,name (,first-arg ,second-arg &key out))
                (define-two-arg-functions ,name ,cl-name (,first-arg ,second-arg)
                    (,single-float-c-name single-float)
                    (,double-float-c-name double-float))
                ;; If someone is worried about the compilation time; then know that that comes
                ;; from this def-test form :/
                (define-numericals-two-arg-test ,name array
                    (,single-float-error ,sf-min ,sf-max single-float)
                    (,double-float-error ,df-min ,df-max double-float)))))

  (def dn:expt cl:expt (base power)
    (c:dn-spow 2f-7)
    (c:dn-dpow 1d-15)))

(macrolet ((def (name cl-name (first-arg second-arg)
                 (single-float-c-name single-float-return-type single-float-error
                  &optional (single-float-return-element-size 4)
                  (sf-min 0.0f0) (sf-max 1.0f0))
                 (double-float-c-name double-float-return-type double-float-error
                  &optional (double-float-return-element-size 8)
                  (df-min 0.0d0) (df-max 1.0d0)))
             (eval `(define-polymorphic-function ,name (&rest args)
                      :overwrite t))
             `(progn
                (define-polymorphic-function ,name (&rest args))
                (define-two-arg-functions ,name ,cl-name (,first-arg ,second-arg)
                    (,single-float-c-name ,single-float-return-type
                     ,single-float-return-element-size)
                    (,double-float-c-name ,double-float-return-type
                     ,double-float-return-element-size))
                ;; If someone is worried about the compilation time; then know that that comes
                ;; from this def-test form :/
                (define-numericals-two-arg-test ,name array
                    (,single-float-error ,sf-min ,sf-max ,single-float-return-type)
                    (,double-float-error ,df-min ,df-max ,double-float-return-type)))))

  (def dn:two-arg-+ cl:+ (x y) (c:dn-sadd single-float 1f-7) (c:dn-dadd double-float 1d-15))
  (def dn:two-arg-- cl:- (x y) (c:dn-ssub single-float 1f-7) (c:dn-dsub double-float 1d-15))
  (def dn:two-arg-* cl:* (x y) (c:dn-smul single-float 1f-7) (c:dn-dmul double-float 1d-15))
  (def dn:two-arg-/ cl:/ (x y) (c:dn-sdiv single-float 1f-7) (c:dn-ddiv double-float 1d-15))

  (def dn:two-arg-< cl:< (x y)
    (c:dn-slt (unsigned-byte 8) 0 1)
    (c:dn-dlt (unsigned-byte 8) 0 1))
  (def dn:two-arg-<= cl:<= (x y)
    (c:dn-sle (unsigned-byte 8) 0 1)
    (c:dn-dle (unsigned-byte 8) 0 1))
  (def dn:two-arg-= cl:= (x y)
    (c:dn-seq (unsigned-byte 8) 0 1)
    (c:dn-deq (unsigned-byte 8) 0 1))
  (def dn:two-arg-/= cl:/= (x y)
    (c:dn-sneq (unsigned-byte 8) 0 1)
    (c:dn-dneq (unsigned-byte 8) 0 1))
  (def dn:two-arg->= cl:>= (x y)
    (c:dn-sge (unsigned-byte 8) 0 1)
    (c:dn-dge (unsigned-byte 8) 0 1))
  (def dn:two-arg-> cl:> (x y)
    (c:dn-sgt (unsigned-byte 8) 0 1)
    (c:dn-dgt (unsigned-byte 8) 0 1)))

(define-two-arg-functions dn:atan cl:atan
    (x y) (c:dn-satan2 single-float) (c:dn-datan2 double-float))

;; (define-numericals-two-arg-test dn:atan array
;;     (2f-7  0.0f0 1.0f0 single-float)
;;     (1d-15 0.0d0 1.0d0 double-float))
