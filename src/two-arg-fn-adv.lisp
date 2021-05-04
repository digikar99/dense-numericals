(define-polymorphic-function dn:copy (x &key out) :overwrite t)
(defpolymorph dn:copy
    ((x (array single-float))  &key
     ((out (array single-float)) (zeros-like x)))
    (array single-float)
  (ptr-iterate-but-inner 4 n ((ptr-x ix x) (ptr-out iout out)) 
                         (linalg.c:cblas-scopy n ptr-x ix ptr-out iout))
  out)


(define-polymorphic-function linalg:axpy (x y &key out a) :overwrite t)

(defpolymorph linalg:axpy
    ((x (array single-float)) (y (array single-float)) &key
     ((out (array single-float)) (zeros-like y))
     ((a single-float) 1.0 ))
    (array single-float)
  (dn:copy y :out out)
  (ptr-iterate-but-inner 4 n ((ptr-x ix x) (ptr-out iout out)) ; x= [[1,2,3],[4,5,6]] y=(2 x 3), in every iter cblax-saxpy(3,x[i],1,y[i],1)
                         (linalg.c:cblas-saxpy n a ptr-x ix ptr-out iout))
  out)

;; (define-polymorphic-function dn:two-arg-+ (x y &key out) :overwrite t)

(defpolymorph dn:two-arg-+
    ((x (array single-float)) (y (array single-float))
     &key ((out (array single-float)) (zeros-like y)))
    (array single-float)
  (linalg:axpy x y :out out)
  out)

(defpolymorph dn:two-arg--
    ((x (array single-float)) (y (array single-float))
     &key ((out (array single-float)) (zeros-like y)))
    (array single-float)
  (linalg:axpy x (dn:two-arg-* -1 y) :out out)
  out)



(defpolymorph dn:two-arg-*
    ((x (array single-float)) (y (array single-float))
     &key ((out (array single-float)) (zeros-like y)))
    (array single-float)
    (ptr-iterate-but-inner 4 n ((ptr-x ix x) (ptr-out iout out)) 
                         (linalg.c:cblas-scopy n ptr-x ix ptr-out iout))

  out)
  
