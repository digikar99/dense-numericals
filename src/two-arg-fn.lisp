(in-package :dense-numericals.impl)
(5am:in-suite :dense-numericals)

;; Basic Concept:
;; (c:dn-ssin (array-total-size x) (ptr x) 1 (ptr out) 1)

(macrolet ((def (name cl-name)
             `(progn
                (define-polymorphic-function ,name (x y &key out) :overwrite t)
                (defpolymorph ,name ((x number) (y number)
                                     &key ((out null)))
                    number
                  (declare (ignore out))
                  (,cl-name x y)))))
  
  (def dn:two-arg-+ +)
  (def dn:two-arg-- -)
  (def dn:two-arg-/ /)
  (def dn:two-arg-* *))


(macrolet ((def (name cl-name)
             `(progn
                (define-polymorphic-function ,name (x y &key out) :overwrite t)
                (defpolymorph ,name ((x number) (y number)
                                     &key ((out null)))
                    bit
                  (declare (ignore out))
                  ;; FIXME: What behavior do we want?
                  (if (,cl-name x y) 1 0)))))
  (def dn:two-arg-<  <)
  (def dn:two-arg-<= <=)
  (def dn:two-arg-=  =)
  (def dn:two-arg-/= /=)
  (def dn:two-arg->  >)
  (def dn:two-arg->= >=))
