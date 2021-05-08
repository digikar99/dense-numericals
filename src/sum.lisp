(in-package :dense-numericals.impl)

;;; numpy is still about a factor 2 fast for large arrays, perhaps because
;;; it might not be using dot under the hood
;;; TODO: Check out Bela Pecsek's simd version
;;; TODO: Let DN:SUM take AXES as a LIST
(define-polymorphic-function dn:sum (x &key axes out) :overwrite t)
(defpolymorph dn:sum ((x (array single-float))
                      &key ((axes null)) ((out null)))
    single-float
  (declare (ignore axes out))
  (let ((sum 0.0f0))
    (cffi:with-foreign-pointer (ones 4)
      (setf (cffi:mem-aref ones :float) 1.0f0)
      (ptr-iterate-but-inner n ((ptr-x 4 inc-x x))
                             (incf sum
                                   (linalg.c:cblas-sdot n
                                                        ptr-x 1
                                                        ones 0))))
    sum))

(defpolymorph dn:sum ((x (array single-float 1))
                      &key ((axes (eql 0))) ((out null)))
    single-float
  (declare (ignore axes out))
  (cffi:with-foreign-pointer (ones 4)
    (setf (cffi:mem-aref ones :float) 1.0f0)
    (linalg.c:cblas-sdot (array-total-size x)
                         (ptr x) 1
                         ones 0)))

(defpolymorph dn:sum ((x (array single-float))
                      &key ((axes integer)) ((out (array single-float))))
    (array single-float)
  ;; TODO: Look towards stack allocating dense-arrays
  (let ((copy-x (apply #'aref x (loop :for i :below (array-rank x)
                                      :if (= i axes)
                                        :collect i
                                      :else
                                        :collect nil)))
        (offset (array-total-size out)))
    (declare (type (array single-float) copy-x))
    (loop :for i :below (array-dimension x axes)
          :do (dn:two-arg-+ out copy-x :out out)
              (incf (first (array-offsets copy-x))
                    offset)))
  out)

;;; The perf diff between simple and non-simple versions is less than 5%
;;; for (RAND 1000 1000); may be we could delete this (?)
(defpolymorph dn:sum ((x (simple-array single-float))
                      &key ((axes null)) ((out null)))
    single-float
  (declare (ignore axes out))
  (cffi:with-foreign-pointer (ones 4)
    (setf (cffi:mem-aref ones :float) 1.0f0)
    (linalg.c:cblas-sdot (array-total-size x)
                         (ptr x) 1
                         ones 0)))

