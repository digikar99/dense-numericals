(in-package :dense-numericals.impl)
(5am:in-suite :dense-numericals)



(define-polymorphic-function linalg:axpy (x y &key out a) :overwrite t)

(defpolymorph linalg:axpy
    ((x (array single-float)) (y (array single-float)) &key
     ((out (array single-float)) (zeros-like y))
     ((a single-float) 1.0 ))
    (array single-float)
  (dn:copy y :out out)
  (ptr-iterate-but-inner n ((ptr-x 4 ix x)
                            (ptr-out 4 iout out))
                         (linalg.c:cblas-saxpy n a ptr-x ix ptr-out iout))
  out)

