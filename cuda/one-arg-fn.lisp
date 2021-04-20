(in-package :dense-numericals.impl)

(defparameter *cuda-sync* t)
(defconstant +max-cuda-threads+ 1024)

(5am:def-suite* cuda-array :in :dense-numericals)

(defmacro define-cuda-one-arg-fn (polymorph-name
                                  single-float-cuda-fn-name double-float-cuda-fn-name)


  ;; TODO: Optimize unsimple-array
  (flet ((unsimple-array (type fn-name)
             `(defpolymorph ,polymorph-name ((a (cuda-array ,type))
                                             &key ((out (cuda-array ,type)) (zeros-like a)))
                  (cuda-array ,type)
                (declare (optimize speed))
                (when *cuda-sync*
                  (dc::sync-memory-block (array-storage a) :host-to-device))
                (device-ptr-iterate-but-inner ,(ecase type
                                                 (single-float 4)
                                                 (double-float 8))
                    n
                    ((dptr-a inc-a a)
                     (dptr-o inc-o out))
                  ;; TODO: We are only invoking a single thread here
                  ;; This helps us avoid "wasteful" threads; ideally
                  ;; we should benchmark here
                  (,fn-name 1 n dptr-a inc-a dptr-o inc-o
                            :grid-dim  '(1 1 1)
                            :block-dim '(1 1 1)))
                (when *cuda-sync*
                  (dc::sync-memory-block (array-storage out) :device-to-host))
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
                (,fn-name n 1 dptr-a 1 dptr-o 1
                          :grid-dim  grid-dim
                          :block-dim block-dim)
                (when *cuda-sync*
                  (dc::sync-memory-block (array-storage out) :device-to-host)))
              out)))

    `(progn
       ,(simple-array 'single-float single-float-cuda-fn-name)
       ,(simple-array 'double-float double-float-cuda-fn-name)
       ,(unsimple-array 'single-float single-float-cuda-fn-name)
       ,(unsimple-array 'double-float double-float-cuda-fn-name))))

(macrolet ((def (name
                 (single-float-cuda-name single-float-error
                  &optional (sf-min 0.0f0) (sf-max 1.0f0))
                 (double-float-cuda-name double-float-error
                  &optional (df-min 0.0d0) (df-max 1.0d0)))
             `(progn
                (define-cuda-one-arg-fn ,name
                  ,single-float-cuda-name ,double-float-cuda-name)
                ;; If someone is worried about the compilation time; then know that that comes
                ;; from this def-test form :/
                (define-numericals-test ,name cuda-array
                    (,single-float-error ,sf-min ,sf-max)
                    (,double-float-error ,df-min ,df-max)))))
  (def dn:sin (dc::ssin 2f-7) (dc::dsin 1d-15))
  (def dn:cos (dc::scos 2f-7) (dc::dcos 1d-15))
  (def dn:tan (dc::stan 2f-7) (dc::dtan 1d-15))

  (def dn:asin (dc::sasin 2f-7) (dc::dasin 1d-15))
  (def dn:acos (dc::sacos 2f-7) (dc::dacos 1d-15))
  (def dn:atan (dc::satan 2f-7) (dc::datan 1d-15)) ; Handle atan case specially

  (def dn:sinh (dc::ssinh 2f-7) (dc::dsinh 1d-15))
  (def dn:cosh (dc::scosh 2f-7) (dc::dcosh 1d-15))
  (def dn:tanh (dc::stanh 3f-7) (dc::dtanh 1d-15))

  (def dn:asinh (dc::sasinh 2f-7) (dc::dasinh 1d-15))
  (def dn:acosh (dc::sacosh 2f-7 1.0f0 2.0f0) (dc::dacosh 1d-15 1.0d0 2.0d0))
  (def dn:atanh (dc::satanh 2f-7) (dc::datanh 1d-15))
  (def dn:exp (dc::sexp 2f-7) (dc::dexp 1d-15))
  (def dn:sqrt (dc::ssqrt 2f-7) (dc::dsqrt 2f-7)))

(5am:def-test async (:suite cuda-array)

  (let* ((*cuda-sync* nil))
    (flet ((float-close-p (x y)
             (< (/ (abs (- x y)) (abs x))
                1f-3)))

      (macrolet ((async-test (forward backward)
                   `(let* ((a (rand 100 100 :min 0.01 :max 0.99))
                           (o (zeros-like a)))
                      (array= a
                              (progn
                                (dc::sync-memory-block (array-storage a) :host-to-device)
                                (,backward (,forward a :out o)
                                           :out o)
                                (dc::sync-memory-block (array-storage o) :device-to-host)
                                o)
                              :test #'float-close-p))))
        (5am:is-true (async-test dn:sin dn:asin))
        (5am:is-true (async-test dn:cos dn:acos))
        (5am:is-true (async-test dn:tan dn:atan)))

      (macrolet ((async-test (forward backward)
                   `(let* ((a (rand 100 100 :min 0.01 :max 0.99))
                           (o (zeros-like a)))
                      (array= a
                              (progn
                                (dc::sync-memory-block (array-storage a) :host-to-device)
                                (,backward (,forward (,backward (,forward a :out o)
                                                                :out o)
                                                     :out o)
                                           :out o)
                                (dc::sync-memory-block (array-storage o) :device-to-host)
                                o)
                              :test #'float-close-p))))
        (5am:is-true (async-test dn:sin dn:asin))
        (5am:is-true (async-test dn:cos dn:acos))
        (5am:is-true (async-test dn:tan dn:atan))))))
