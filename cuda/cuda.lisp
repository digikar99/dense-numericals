(in-package :dense-numericals.cuda)

;; (CL-CUDA.API.KERNEL-MANAGER:KERNEL-MANAGER-UNLOAD
;;          CL-CUDA.API.KERNEL-MANAGER:*KERNEL-MANAGER*)

;; (with-cuda (0))

(macrolet ((def (single-float-fn-name double-float-fn-name cl-name)
             `(progn
                (defkernel ,single-float-fn-name (void ((n-global int)
                                                        (n-local  int)
                                                        (a float*)   (inc-a int)
                                                        (out float*) (inc-o int)))
                  (do ((i-start (+ (* block-dim-x block-idx-x)
                                   thread-idx-x)
                                (+ i-start (* block-dim-x grid-dim-x))))
                      ((>= i-start n-global))
                    (do ((i 0 (+ i 1)))
                        ((>= i n-local))
                      (set (aref out (+ i-start (* i inc-a)))
                           (,cl-name (aref a (+ i-start (* i inc-o))))))))
                (defkernel ,double-float-fn-name (void ((n-global int)
                                                        (n-local  int)
                                                        (a double*)   (inc-a int)
                                                        (out double*) (inc-o int)))
                  (do ((i-start (+ (* block-dim-x block-idx-x)
                                   thread-idx-x)
                                (+ i-start (* block-dim-x grid-dim-x))))
                      ((>= i-start n-global))
                    (do ((i 0 (+ i 1)))
                        ((>= i n-local))
                      (set (aref out (+ i-start (* i inc-a)))
                           (,cl-name (aref a (+ i-start (* i inc-o)))))))))))
  ;; Is there a limit to the number of blocks?
  ;; Should we attempt a grid-stride loop?

  (def ssin   dsin   sin)
  (def scos   dcos   cos)
  (def stan   dtan   tan)
  (def sasin  dasin  asin)
  (def sacos  dacos  acos)
  (def satan  datan  atan)
  (def ssinh  dsinh  sinh)
  (def scosh  dcosh  cosh)
  (def stanh  dtanh  tanh)
  (def sasinh dasinh asinh)
  (def sacosh dacosh acosh)
  (def satanh datanh atanh)

  (def ssqrt  dsqrt  sqrt)
  (def sexp   dexp   exp))



(macrolet ((def (cl-name
                 (single-float-fn-name single-float-return-type)
                 (double-float-fn-name double-float-return-type))
             `(progn
                (defkernel ,single-float-fn-name (void ((n-global int)
                                                        (n-local  int)
                                                        (a float*) (inc-a int)
                                                        (b float*) (inc-b int)
                                                        (o ,single-float-return-type)
                                                        (inc-o int)))
                  (do ((i-start (+ (* block-dim-x block-idx-x)
                                   thread-idx-x)
                                (+ i-start (* block-dim-x grid-dim-x))))
                      ((>= i-start n-global))
                    (do ((i 0 (+ i 1)))
                        ((>= i n-local))
                      (set (aref o (+ i-start (* i inc-o)))
                           (,cl-name (aref a (+ i-start (* i inc-a)))
                                     (aref b (+ i-start (* i inc-b))))))))
                (defkernel ,double-float-fn-name (void ((n-global int)
                                                        (n-local  int)
                                                        (a double*) (inc-a int)
                                                        (b double*) (inc-b int)
                                                        (o ,double-float-return-type)
                                                        (inc-o int)))
                  (do ((i-start (+ (* block-dim-x block-idx-x)
                                   thread-idx-x)
                                (+ i-start (* block-dim-x grid-dim-x))))
                      ((>= i-start n-global))
                    (do ((i 0 (+ i 1)))
                        ((>= i n-local))
                      (set (aref o (+ i-start (* i inc-o)))
                           (,cl-name (aref a (+ i-start (* i inc-a)))
                                     (aref b (+ i-start (* i inc-b)))))))))))
  ;; Is there a limit to the number of blocks?
  ;; Should we attempt a grid-stride loop?

  (def atan (satan2 float*) (datan2 double*))

  (def + (s-add      float*) (d-add      double*))
  (def - (s-subtract float*) (d-subtract double*))
  (def / (s-divide   float*) (d-divide   double*))
  (def * (s-multiply float*) (d-multiply double*))

  (def <  (s-less-p          bool*) (d-less-p          bool*))
  (def <= (s-less-equal-p    bool*) (d-less-equal-p    bool*))
  (def =  (s-equal-p         bool*) (d-equal-p         bool*))
  (def >  (s-greater-p       bool*) (d-greater-p       bool*))
  (def >= (s-greater-equal-p bool*) (d-greater-equal-p bool*)))


