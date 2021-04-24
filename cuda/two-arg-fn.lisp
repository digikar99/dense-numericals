(in-package :dense-numericals.impl)

(defmacro define-cuda-two-arg-fn (polymorph-name
                                  (single-float-cuda-fn-name single-float-return-type)
                                  (double-float-cuda-fn-name double-float-return-type))


  ;; TODO: Optimize unsimple-array
  (flet ((unsimple-array (type fn-name return-type)
           `(defpolymorph ,polymorph-name ((a (cuda-array ,type)) (b (cuda-array ,type))
                                           ;; TODO: Broadcasting?
                                           &key ((out (cuda-array ,return-type))
                                                 (zeros (array-dimensions a)
                                                        :type ',return-type)))
                (cuda-array ,return-type)
              (declare (optimize speed))
              (when *cuda-sync*
                (dc::sync-memory-block (array-storage a) :host-to-device)
                (dc::sync-memory-block (array-storage b) :host-to-device))
              (device-ptr-iterate-but-inner ,(ecase type
                                               (single-float 4)
                                               (double-float 8))
                  n
                  ((dptr-a inc-a a)
                   (dptr-b inc-b b)
                   (dptr-o inc-o out))
                ;; TODO: We are only invoking a single thread here
                ;; This helps us avoid "wasteful" threads; ideally
                ;; we should benchmark here
                (,fn-name 1 n dptr-a inc-a dptr-b inc-b dptr-o inc-o
                          :grid-dim  '(1 1 1)
                          :block-dim '(1 1 1)))
              (when *cuda-sync*
                (dc::sync-memory-block (array-storage out) :device-to-host))
              out))
         (simple-array (type fn-name return-type)
           `(defpolymorph ,polymorph-name ((a (simple-cuda-array ,type))
                                           (b (simple-cuda-array ,type))
                                           &key ((out (simple-cuda-array ,return-type))
                                                 (zeros (array-dimensions a)
                                                        :type ',return-type)))
                (simple-cuda-array ,return-type)
              (declare (optimize speed))
              (let* ((n (array-total-size a))
                     (dptr-a (dc::memory-block-device-ptr (array-storage a)))
                     (dptr-b (dc::memory-block-device-ptr (array-storage b)))
                     (dptr-o (dc::memory-block-device-ptr (array-storage out)))
                     (block-dim (list (min n +max-cuda-threads+) 1 1))
                     (grid-dim  (list (1+ (floor n +max-cuda-threads+)) 1 1)))
                (declare (dynamic-extent block-dim grid-dim))
                (when *cuda-sync*
                  (dc::sync-memory-block (array-storage a) :host-to-device)
                  (dc::sync-memory-block (array-storage b) :host-to-device))
                (,fn-name n 1 dptr-a 1 dptr-b 1 dptr-o 1
                          :grid-dim  grid-dim
                          :block-dim block-dim)
                (when *cuda-sync*
                  (dc::sync-memory-block (array-storage out) :device-to-host)))
              out)))

    `(progn
       ,(simple-array 'single-float single-float-cuda-fn-name single-float-return-type)
       ,(simple-array 'double-float double-float-cuda-fn-name double-float-return-type)
       ,(unsimple-array 'single-float single-float-cuda-fn-name single-float-return-type)
       ,(unsimple-array 'double-float double-float-cuda-fn-name double-float-return-type))))

(macrolet ((def (name
                 (single-float-return-type single-float-cuda-name single-float-error
                  &optional (sf-min 0.0f0) (sf-max 1.0f0))
                 (double-float-return-type double-float-cuda-name double-float-error
                  &optional (df-min 0.0d0) (df-max 1.0d0)))
             `(progn
                (define-cuda-two-arg-fn ,name
                    (,single-float-cuda-name ,single-float-return-type)
                    (,double-float-cuda-name ,double-float-return-type))
                ;; If someone is worried about the compilation time; then know that that comes
                ;; from this def-test form :/
                (define-numericals-two-arg-test ,name cuda-array
                    (,single-float-error ,sf-min ,sf-max ,single-float-return-type)
                    (,double-float-error ,df-min ,df-max ,double-float-return-type)))))

  (def dn:two-arg-+ (single-float dc::s-add      2f-7) (double-float dc::d-add      1d-15))
  (def dn:two-arg-- (single-float dc::s-subtract 2f-7) (double-float dc::d-subtract 1d-15))
  (def dn:two-arg-/ (single-float dc::s-divide   2f-7) (double-float dc::d-divide   1d-15))
  (def dn:two-arg-* (single-float dc::s-multiply 2f-7) (double-float dc::d-multiply 1d-15))
 
  (def dn:two-arg-<  (bit dc::s-less-p          2f-7) (bit dc::d-less-p          1d-15))
  (def dn:two-arg-<= (bit dc::s-less-equal-p    2f-7) (bit dc::d-less-equal-p    1d-15))
  (def dn:two-arg-=  (bit dc::s-equal-p         2f-7) (bit dc::d-equal-p         1d-15))
  (def dn:two-arg->  (bit dc::s-greater-p       2f-7) (bit dc::d-greater-p       1d-15))
  (def dn:two-arg->= (bit dc::s-greater-equal-p 2f-7) (bit dc::d-greater-equal-p 1d-15)))

(define-cuda-two-arg-fn dn:atan (dc::satan2 single-float) (dc::datan2 double-float))
;; TODO: Tests for ATAN2
