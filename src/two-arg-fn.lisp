(in-package :dense-numericals.impl)
(5am:in-suite :dense-numericals)

;; Basic Concept:
;; (bmas:bmas-ssin (array-total-size x) (ptr x) 1 (ptr out) 1)

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
       ;; TODO: Handle cases reducible to exp2, exp and exp10
       (with-thresholded-multithreading (array-total-size
                                         (the (array ,single-float-return-type) out))
           (,first-arg out)
         (cffi:with-foreign-pointer (ptr-y 4)
           (setf (cffi:mem-ref ptr-y :float) (coerce ,second-arg 'single-float))
           (ptr-iterate-but-inner n ((ptr-x 4                                 ix ,first-arg)
                                     (ptr-o ,single-float-return-element-size io out))
             (,single-float-c-name n
                                   ptr-x ix
                                   ptr-y 0
                                   ptr-o io))))
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
                          &key ((out (or null (array ,single-float-return-type)))))
         (array ,single-float-return-type)
       ;; TODO: Handle cases reducible to exp2, exp and exp10
       (unless (and out
                    (equalp (narray-dimensions ,first-arg)
                            (narray-dimensions ,second-arg)))
         (multiple-value-bind (broadcast-compatible-p broadcast-dimensions)
             (broadcast-compatible-p ,first-arg ,second-arg)
           (assert broadcast-compatible-p (,first-arg ,second-arg)
                   'incompatible-broadcast-dimensions
                   :dimensions (mapcar #'narray-dimensions (list ,first-arg ,second-arg))
                   :array-likes (list ,first-arg ,second-arg))
           (setq ,first-arg  (broadcast-array ,first-arg broadcast-dimensions))
           (setq ,second-arg (broadcast-array ,second-arg broadcast-dimensions))
           (setq out (or out (zeros broadcast-dimensions :type ',single-float-return-type)))))
       (with-thresholded-multithreading (array-total-size
                                         (the (array ,single-float-return-type) out))
           (,first-arg ,second-arg out)
         (ptr-iterate-but-inner n ((ptr-x 4                                 ix ,first-arg)
                                   (ptr-y 4                                 iy ,second-arg)
                                   (ptr-o ,single-float-return-element-size io out))
           (,single-float-c-name n
                                 ptr-x ix
                                 ptr-y iy
                                 ptr-o io)))
       out)

     (defpolymorph (,name :inline t)
         ((,first-arg (simple-array single-float))
          (,second-arg (simple-array single-float))
          &key ((out (or null (simple-array ,single-float-return-type)))))
         (array ,single-float-return-type)
       ;; TODO: Handle cases reducible to exp2, exp and exp10
       (if (and out
                (equalp (narray-dimensions ,first-arg)
                        (narray-dimensions ,second-arg))
                (equalp (narray-dimensions out)
                        (narray-dimensions ,second-arg)))
           (with-thresholded-multithreading (array-total-size (the array out))
               (:simple ,first-arg ,second-arg out)
             (,single-float-c-name (array-total-size (the array out))
                                   (ptr ,first-arg 4) 1
                                   (ptr ,second-arg 4) 1
                                   (ptr out 4) 1))
           (multiple-value-bind (broadcast-compatible-p broadcast-dimensions)
               (broadcast-compatible-p ,first-arg ,second-arg)
             (assert broadcast-compatible-p (,first-arg ,second-arg)
                     'incompatible-broadcast-dimensions
                     :dimensions (mapcar #'narray-dimensions (list ,first-arg ,second-arg))
                     :array-likes (list ,first-arg ,second-arg))
             (setq ,first-arg  (broadcast-array ,first-arg broadcast-dimensions))
             (setq ,second-arg (broadcast-array ,second-arg broadcast-dimensions))
             (setq out (or out (zeros broadcast-dimensions :type ',single-float-return-type)))
             (with-thresholded-multithreading (array-total-size
                                               (the (array ,single-float-return-type) out))
                 (,first-arg ,second-arg out)
               (ptr-iterate-but-inner n ((ptr-x 4                                 ix ,first-arg)
                                         (ptr-y 4                                 iy ,second-arg)
                                         (ptr-o ,single-float-return-element-size io out))
                 (,single-float-c-name n
                                       ptr-x ix
                                       ptr-y iy
                                       ptr-o io)))))
       out)



     ;; double-float - 4 polymorphs

     (defpolymorph ,name ((,first-arg (array double-float)) (,second-arg number)
                          &key ((out (array ,double-float-return-type))
                                (zeros (narray-dimensions ,first-arg)
                                       :type ',double-float-return-type)))
         (array ,double-float-return-type)
       ;; TODO: Handle cases reducible to exp2, exp and exp10
       (with-thresholded-multithreading (array-total-size
                                         (the (array ,double-float-return-type) out))
           (,first-arg out)
         (cffi:with-foreign-pointer (ptr-y 8)
           (setf (cffi:mem-ref ptr-y :double) (coerce ,second-arg 'double-float))
           (ptr-iterate-but-inner n ((ptr-x 8                                 ix ,first-arg)
                                     (ptr-o ,double-float-return-element-size io out))
             (,double-float-c-name n
                                   ptr-x ix
                                   ptr-y 0
                                   ptr-o io))))
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
                                (zeros-like ,first-arg)))
         (array ,double-float-return-type)
       ;; TODO: Handle cases reducible to exp2, exp and exp10
       (unless (and out
                    (equalp (narray-dimensions ,first-arg)
                            (narray-dimensions ,second-arg)))
         (multiple-value-bind (broadcast-compatible-p broadcast-dimensions)
             (broadcast-compatible-p ,first-arg ,second-arg)
           (assert broadcast-compatible-p (,first-arg ,second-arg)
                   'incompatible-broadcast-dimensions
                   :dimensions (mapcar #'narray-dimensions (list ,first-arg ,second-arg))
                   :array-likes (list ,first-arg ,second-arg))
           (setq ,first-arg  (broadcast-array ,first-arg broadcast-dimensions))
           (setq ,second-arg (broadcast-array ,second-arg broadcast-dimensions))
           (setq out (or out (zeros broadcast-dimensions :type ',double-float-return-type)))))
       (with-thresholded-multithreading (array-total-size
                                         (the (array ,double-float-return-type) out))
           (,first-arg ,second-arg out)
         (ptr-iterate-but-inner n ((ptr-x 8                                 ix ,first-arg)
                                   (ptr-y 8                                 iy ,second-arg)
                                   (ptr-o ,double-float-return-element-size io out))
                (,double-float-c-name n
                                      ptr-x ix
                                      ptr-y iy
                                      ptr-o io)))
       out)

     (defpolymorph (,name :inline t)
         ((,first-arg (simple-array double-float))
          (,second-arg (simple-array double-float))
          &key ((out (or null (simple-array ,double-float-return-type)))))
         (array ,double-float-return-type)
       ;; TODO: Handle cases reducible to exp2, exp and exp10
       (if (and out
                (equalp (narray-dimensions ,first-arg)
                        (narray-dimensions ,second-arg))
                (equalp (narray-dimensions out)
                        (narray-dimensions ,second-arg)))
           (with-thresholded-multithreading (array-total-size (the array out))
               (:simple ,first-arg ,second-arg out)
             (,double-float-c-name (array-total-size (the array out))
                                   (ptr ,first-arg 8) 1
                                   (ptr ,second-arg 8) 1
                                   (ptr out 8) 1))
           (multiple-value-bind (broadcast-compatible-p broadcast-dimensions)
               (broadcast-compatible-p ,first-arg ,second-arg)
             (assert broadcast-compatible-p (,first-arg ,second-arg)
                     'incompatible-broadcast-dimensions
                     :dimensions (mapcar #'narray-dimensions (list ,first-arg ,second-arg))
                     :array-likes (list ,first-arg ,second-arg))
             (setq ,first-arg  (broadcast-array ,first-arg broadcast-dimensions))
             (setq ,second-arg (broadcast-array ,second-arg broadcast-dimensions))
             (setq out (or out (zeros broadcast-dimensions :type ',double-float-return-type)))
             (with-thresholded-multithreading (array-total-size
                                               (the (array ,double-float-return-type) out))
                 (,first-arg ,second-arg out)
               (ptr-iterate-but-inner n ((ptr-x 8                                 ix ,first-arg)
                                         (ptr-y 8                                 iy ,second-arg)
                                         (ptr-o ,double-float-return-element-size io out))
                 (,double-float-c-name n
                                       ptr-x ix
                                       ptr-y iy
                                       ptr-o io)))))
       out)))


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
    (bmas:spow 2f-7)
    (bmas:dpow 1d-15)))

(define-two-arg-functions dn:atan cl:atan
    (x y) (bmas:satan2 single-float) (bmas:datan2 double-float))

;; (define-numericals-two-arg-test dn:atan array
;;     (2f-7  0.0f0 1.0f0 single-float)
;;     (1d-15 0.0d0 1.0d0 double-float))
