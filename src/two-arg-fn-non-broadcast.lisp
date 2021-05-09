(in-package :dense-numericals.impl)

;;; We let the n-arg version handle the broadcasting

(defmacro define-two-arg-functions/non-broadcast

    (name cl-name
     (first-arg second-arg)
     (single-float-c-name single-float-return-type
      &optional (single-float-return-element-size 4))
     (double-float-c-name double-float-return-type
      &optional (double-float-return-element-size 8)))
  
  `(progn

     ;; Pure number
     (defpolymorph ,name ((,first-arg number) (,second-arg number) &key ((out null) nil))
         number
       (declare (ignore out))
       ,(if (member cl-name '(cl:< cl:= cl:<= cl:/= cl:>= cl:>))
            `(if (,cl-name ,first-arg ,second-arg)
                 1
                 0)
            `(,cl-name ,first-arg ,second-arg)))


     ;; list - 3x2 polymorphs: we do need the two variants
     ;;   because below, the OUT is initialized in the lambda-list itself
     (defpolymorph (,name :inline t)
         ((,first-arg list) (,second-arg list) &key ((out array)))
         (values array &optional)
       (,name (asarray ,first-arg) (asarray ,second-arg) :out out))
     (defpolymorph (,name :inline t)
         ((,first-arg number) (,second-arg list) &key ((out array)))
         (values array &optional)
       (,name ,first-arg (asarray ,second-arg) :out out))
     (defpolymorph (,name :inline t)
         ((,first-arg list) (,second-arg number) &key ((out array)))
         (values array &optional)
       (,name (asarray ,first-arg) ,second-arg :out out))

     (defpolymorph (,name :inline t)
         ((,first-arg list) (,second-arg list) &key ((out null)))
         (values array &optional)
       (declare (ignore out))
       (,name (asarray ,first-arg) (asarray ,second-arg)))
     (defpolymorph (,name :inline t)
         ((,first-arg number) (,second-arg list) &key ((out null)))
         (values array &optional)
       (declare (ignore out))
       (,name ,first-arg (asarray ,second-arg)))
     (defpolymorph (,name :inline t)
         ((,first-arg list) (,second-arg number) &key ((out null)))
         (values array &optional)
       (declare (ignore out))
       (,name (asarray ,first-arg) ,second-arg))
     

     
     ;; single-float - 4 polymorphs

     (defpolymorph ,name ((,first-arg (array single-float)) (,second-arg number)
                          &key ((out (array ,single-float-return-type))
                                (zeros (narray-dimensions ,first-arg)
                                       :type ',single-float-return-type)))
         (array ,single-float-return-type)
       ;; TODO: Handle broadcasting
       ;; TODO: Handle cases reducible to exp2, exp and exp10
       (cffi:with-foreign-pointer (ptr-y 4)
         (setf (cffi:mem-ref ptr-y :float) (coerce ,second-arg 'single-float))
         (ptr-iterate-but-inner n ((ptr-x 4                                 ix ,first-arg)
                                   (ptr-o ,single-float-return-element-size io out))
                                (,single-float-c-name n
                                                      ptr-x ix
                                                      ptr-y 0
                                                      ptr-o io)))
       out)

     (defpolymorph (,name :inline t)
         ((,first-arg number) (,second-arg (array single-float))
          &key ((out (array ,single-float-return-type))
                (zeros (narray-dimensions ,second-arg)
                       :type ',single-float-return-type)))
         (array ,single-float-return-type)
       (,name ,second-arg ,first-arg :out out)
       out)
     
     
     (defpolymorph ,name ((,first-arg (array single-float)) (,second-arg (array single-float))
                          &key ((out (array ,single-float-return-type))
                                (zeros (narray-dimensions ,first-arg)
                                       :type ',single-float-return-type)))
         (array ,single-float-return-type)
       ;; TODO: Handle cases reducible to exp2, exp and exp10
       (policy-cond:with-expectations (= safety 0)
           ((assertion (equalp (narray-dimensions ,first-arg)
                               (narray-dimensions ,second-arg))))
         (ptr-iterate-but-inner n ((ptr-x 4                                 ix ,first-arg)
                                   (ptr-y 4                                 iy ,second-arg)
                                   (ptr-o ,single-float-return-element-size io out))
                                (,single-float-c-name n
                                                      ptr-x ix
                                                      ptr-y iy
                                                      ptr-o io)))
       out)

     (defpolymorph ,name ((,first-arg (simple-array single-float))
                          (,second-arg (simple-array single-float))
                          &key ((out (simple-array ,single-float-return-type))
                                (zeros (narray-dimensions ,first-arg)
                                       :type ',single-float-return-type)))
         (simple-array ,single-float-return-type)
       ;; TODO: Handle cases reducible to exp2, exp and exp10
       (policy-cond:with-expectations (= safety 0)
           ((assertion (equalp (narray-dimensions ,first-arg)
                               (narray-dimensions ,second-arg))))
         (let ((ptr-x (ptr ,first-arg))
               (ptr-y (ptr ,second-arg))
               (ptr-o (ptr out))
               (n     (array-total-size ,first-arg)))
           (,single-float-c-name n
                                 ptr-x 1
                                 ptr-y 1
                                 ptr-o 1)))
       out)


     
     ;; double-float - 4 polymorphs

     (defpolymorph ,name ((,first-arg (array double-float)) (,second-arg number)
                          &key ((out (array ,double-float-return-type))
                                (zeros (narray-dimensions ,first-arg)
                                       :type ',double-float-return-type)))
         (array ,double-float-return-type)
       ;; TODO: Handle cases reducible to exp2, exp and exp10
       (cffi:with-foreign-pointer (ptr-y 8)
         (setf (cffi:mem-ref ptr-y :double) (coerce ,second-arg 'double-float))
         (ptr-iterate-but-inner n ((ptr-x 8                                 ix ,first-arg)
                                   (ptr-o ,double-float-return-element-size io out))
                                (,double-float-c-name n
                                                      ptr-x ix
                                                      ptr-y 0
                                                      ptr-o io)))
       out)

     (defpolymorph (,name :inline t)
         ((,first-arg number) (,second-arg (array double-float))
          &key ((out (array ,double-float-return-type))
                (zeros (narray-dimensions ,second-arg)
                       :type ',double-float-return-type)))
         (array ,double-float-return-type)
       (,name ,second-arg ,first-arg :out out)
       out)

     (defpolymorph ,name ((,first-arg (array double-float)) (,second-arg (array double-float))
                          &key ((out (array ,double-float-return-type))
                                (zeros (narray-dimensions ,first-arg)
                                       :type ',double-float-return-type)))
         (array ,double-float-return-type)
       ;; TODO: Handle cases reducible to exp2, exp and exp10
       (policy-cond:with-expectations (= safety 0)
           ((assertion (equalp (narray-dimensions ,first-arg)
                               (narray-dimensions ,second-arg))))
         (ptr-iterate-but-inner n ((ptr-x 8                                 ix ,first-arg)
                                   (ptr-y 8                                 iy ,second-arg)
                                   (ptr-o ,double-float-return-element-size io out))
                                (,double-float-c-name n
                                                      ptr-x ix
                                                      ptr-y iy
                                                      ptr-o io)))
       out)

     (defpolymorph ,name ((,first-arg (simple-array double-float))
                          (,second-arg (simple-array double-float))
                          &key ((out (simple-array ,double-float-return-type))
                                (zeros (narray-dimensions ,first-arg)
                                       :type ',double-float-return-type)))
         (simple-array ,double-float-return-type)
       ;; TODO: Handle cases reducible to exp2, exp and exp10
       (policy-cond:with-expectations (= safety 0)
           ((assertion (equalp (narray-dimensions ,first-arg)
                               (narray-dimensions ,second-arg))))
         (let ((ptr-x (ptr ,first-arg))
               (ptr-y (ptr ,second-arg))
               (ptr-o (ptr out))
               (n     (array-total-size ,first-arg)))
           (,double-float-c-name n
                                 ptr-x 1
                                 ptr-y 1
                                 ptr-o 1)))
       out)))

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
                (define-two-arg-functions/non-broadcast ,name ,cl-name (,first-arg ,second-arg)
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


