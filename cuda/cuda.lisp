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

(defkernel satan2
    (void ((n int) (a float*) (b float*) (out float*)))
  (let ((i (+ (* block-dim-x block-idx-x) thread-idx-x)))
    (when (< i n) (set (aref out i) (atan (aref a i) (aref b i))))))

(defkernel datan2
    (void ((n int) (a double*) (b double*) (out double*)))
  (let ((i (+ (* block-dim-x block-idx-x) thread-idx-x)))
    (when (< i n) (set (aref out i) (atan (aref a i) (aref b i))))))

;; (defkernel our-add-kernel (void ((a float*) (b float*) (c float*) (n int)))
;;   (let ((i (+ (* block-dim-x block-idx-x) thread-idx-x)))
;;     (when (< i n)
;;       (set (aref c i)
;;            (+ (aref a i) (aref b i))))))

