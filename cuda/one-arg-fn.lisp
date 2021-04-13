(in-package :dense-numericals.impl)

(defparameter *cuda-sync* t)
(defconstant +max-cuda-threads+ 1024)

(5am:def-suite* cuda-array :in :dense-numericals)

;;; TODO: Optimize

(defmacro define-cuda-one-arg-fn (polymorph-name
                                  single-float-cuda-fn-name double-float-cuda-fn-name)

  (flet ((unsimple-array (type fn-name)
           `(defpolymorph ,polymorph-name ((a (cuda-array ,type))
                                           &key ((out (cuda-array ,type)) (zeros-like a)))
                (cuda-array ,type)
              (declare (optimize speed))
              (let ((grid-dim  (list nil 1 1))
                    (block-dim (list nil 1 1)))
                (declare (dynamic-extent grid-dim block-dim)
                         (optimize speed))
                (when *cuda-sync*
                  (dc::sync-memory-block (array-storage a) :host-to-device))
                (device-ptr-iterate-but-inner 4 n ((dptr-a inc-a a)
                                                   (dptr-o inc-o out))
                  (setf (first grid-dim)  (1+ (floor n +max-cuda-threads+))
                        (first block-dim) (min n +max-cuda-threads+))
                  (,fn-name n dptr-a dptr-o
                            :grid-dim  grid-dim
                            :block-dim block-dim))
                (when *cuda-sync*
                  (dc::sync-memory-block (array-storage out) :device-to-host)))
              out))
         (simple-array (type fn-name)
           `(defpolymorph ,polymorph-name ((a (simple-cuda-array ,type))
                                           &key ((out (simple-cuda-array ,type)) (zeros-like a)))
                (simple-cuda-array ,type)
              (declare (optimize speed))
              (let* ((n (array-total-size a))
                     (dptr-a (dc::memory-block-device-ptr (array-storage a)))
                     (dptr-o (dc::memory-block-device-ptr (array-storage out)))
                     (block-dim (list (min n +max-cuda-threads+) 1 1))
                     (grid-dim  (list (1+ (floor n +max-cuda-threads+)) 1 1)))
                (declare (dynamic-extent block-dim grid-dim))
                (when *cuda-sync*
                  (dc::sync-memory-block (array-storage a) :host-to-device))
                (,fn-name n dptr-a dptr-o
                          :grid-dim  grid-dim
                          :block-dim block-dim)
                (when *cuda-sync*
                  (dc::sync-memory-block (array-storage out) :device-to-host)))
              out)))

    `(progn
       ,(simple-array 'single-float single-float-cuda-fn-name)
       ,(simple-array 'double-float double-float-cuda-fn-name)
       ,(unsimple-array 'single-float single-float-cuda-fn-name)
       ,(unsimple-array 'double-float double-float-cuda-fn-name)

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
           (verify 'double-float ,double-float-error))))))

(macrolet ((def (name (single-float-cuda-name single-float-error)
                      (double-float-cuda-name double-float-error))
             `(progn
                (define-cuda-one-arg-fn ,name
                  ,single-float-cuda-name ,double-float-cuda-name)
                (define-numericals-test ,name cuda-array
                  ,single-float-error ,double-float-error))))

  (def dn:sin (dc::ssin 2f-7) (dc::dsin 1d-15))
  (def dn:cos (dc::scos 2f-7) (dc::dcos 1d-15))
  (def dn:tan (dc::stan 2f-7) (dc::dtan 1d-15))
  ;; \(dc.*?\) \(dc.*?\))
  (def dn:asin (dc::sasin 2f-7) (dc::dasin 1d-15))
  (def dn:acos (dc::sacos 2f-7) (dc::dacos 1d-15))
  (def dn:atan (dc::satan 2f-7) (dc::datan 1d-15))

  (def dn:sinh (dc::ssinh 2f-7) (dc::dsinh 1d-15))
  (def dn:cosh (dc::scosh 2f-7) (dc::dcosh 1d-15))
  (def dn:tanh (dc::stanh 2f-7) (dc::dtanh 1d-15))

  (def dn:asinh (dc::sasinh 2f-7) (dc::dasinh 1d-15))
  (def dn:acosh (dc::sacosh 2f-7) (dc::dacosh 1d-15))
  (def dn:atanh (dc::satanh 2f-7) (dc::datanh 1d-15)))
