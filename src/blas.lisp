(in-package :dense-numericals.impl)

(5am:in-suite array)

;; The first is slower on the author's PC :/ - 10 times as slower as numpy
;; (cffi:load-foreign-library #p"/usr/lib/x86_64-linux-gnu/libblas.so")
;; The second is fast, much faster :D - at par with numpy
(cffi:load-foreign-library #p"/usr/lib/x86_64-linux-gnu/libopenblas.so.0")
;; The miniconda equivalents aren't faster than this for DN:SUM below

(define-polymorphic-function dn:copy (x &key out) :overwrite t)
(defpolymorph dn:copy
    ((x (array single-float))  &key
     ((out (array single-float)) (zeros-like x)))
    (array single-float)
  (ptr-iterate-but-inner n ((ptr-x 4 ix x)
                            (ptr-o 4 iout out))
                         (linalg.c:cblas-scopy n ptr-x ix ptr-o iout))
  out)

(defpolymorph dn:copy
    ((x (array double-float))  &key
     ((out (array double-float)) (zeros-like x)))
    (array double-float)
  (ptr-iterate-but-inner n ((ptr-x 8 ix x)
                            (ptr-o 8 iout out))
                         (linalg.c:cblas-dcopy n ptr-x ix ptr-o iout))
  out)

;; (let ((a (asarray '((1 2 3))))
;;             (b (asarray '((1) (2) (3))))
;;             (c (zeros 1 1)))
;;         (dn:two-arg-matmul a b :out c))

(define-polymorphic-function dn:two-arg-matmul (a b &key out) :overwrite t)

(macrolet ((def (element-type c-fn)
             `(defpolymorph dn:two-arg-matmul ((a (simple-array ,element-type 2))
                                               (b (simple-array ,element-type 2))
                                               &key ((out (simple-array ,element-type 2))))
                  (simple-array ,element-type 2)
                ;; TODO: Generalize this to more dimensions
                (flet ((matmul-compatible-arrays (a b out)
                         (let ((a0 (array-dimension a 0))
                               (a1 (array-dimension a 1))
                               (b0 (array-dimension b 0))
                               (b1 (array-dimension b 1))
                               (o0 (array-dimension out 0))
                               (o1 (array-dimension out 1)))
                           (and (= a0 o0) (= a1 b0) (= b1 o1)))))
                  (policy-cond:with-expectations (= 0 safety)
                      ((assertion (matmul-compatible-arrays a b out) (a b out)))
                    (let ((m (array-dimension a 0))
                          (k (array-dimension a 1))
                          (n (array-dimension b 1)))
                      (,c-fn linalg.c:+cblas-col-major+
                             linalg.c:+cblas-no-trans+
                             linalg.c:+cblas-no-trans+
                             m n k
                             (coerce 1 ',element-type)
                             (ptr a) m
                             (ptr b) k
                             (coerce 0 ',element-type)
                             (ptr out) m))))
                out)))
  
  (def single-float linalg.c:cblas-sgemm)
  (def double-float linalg.c:cblas-dgemm))



(define-polymorphic-function dn:dot (a b &key out) :overwrite t)
(defpolymorph dn:dot ((a (array single-float 1))
                      (b (array single-float 1))
                      &key out)
    single-float
  (declare (ignore out))
  ;; TODO: Generalize this to more dimensions
  (let ((sum 0.0f0))
    (ptr-iterate-but-inner n ((ptr-a 4 inc-a a)
                              (ptr-b 4 inc-b b))
                           (incf sum (linalg.c:cblas-sdot n
                                                          ptr-a inc-a
                                                          ptr-b inc-b)))
    sum))

(defpolymorph dn:dot ((a (simple-array single-float 1))
                      (b (simple-array single-float 1))
                      &key out)
    t
  (declare (ignore out))
  ;; TODO: Generalize this to more dimensions
  (linalg.c:cblas-sdot (array-total-size a)
                       (ptr a) 1
                       (ptr b) 1))
