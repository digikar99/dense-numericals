(in-package :dense-numericals.cuda)

;; (CL-CUDA.API.KERNEL-MANAGER:KERNEL-MANAGER-UNLOAD
;;          CL-CUDA.API.KERNEL-MANAGER:*KERNEL-MANAGER*)

;; (with-cuda (0))

(macrolet ((def (single-float-fn-name double-float-fn-name cl-name)
             `(progn
                (defkernel ,single-float-fn-name (void ((n int) (a float*) (out float*)))
                  (let ((i (+ (* block-dim-x block-idx-x) thread-idx-x)))
                    (when (< i n)
                      (set (aref out i)
                           (,cl-name (aref a i))))))
                (defkernel ,double-float-fn-name (void ((n int) (a double*) (out double*)))
                  (let ((i (+ (* block-dim-x block-idx-x) thread-idx-x)))
                    (when (< i n)
                      (set (aref out i)
                           (,cl-name (aref a i)))))))))
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
  (def satanh datanh atanh))

(defkernel satan2
    (void ((n int) (a float*) (b float*) (out float*)))
  (let ((i (+ (* block-dim-x block-idx-x) thread-idx-x)))
    (when (< i n) (set (aref out i) (atan (aref a i) (aref b i))))))

(defkernel datan2
    (void ((n int) (a double*) (b double*)(out double*)))
  (let ((i (+ (* block-dim-x block-idx-x) thread-idx-x)))
    (when (< i n) (set (aref out i) (atan (aref a i) (aref b i))))))

;; (defkernel our-add-kernel (void ((a float*) (b float*) (c float*) (n int)))
;;   (let ((i (+ (* block-dim-x block-idx-x) thread-idx-x)))
;;     (when (< i n)
;;       (set (aref c i)
;;            (+ (aref a i) (aref b i))))))

