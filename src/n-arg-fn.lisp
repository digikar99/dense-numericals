(in-package :dense-numericals.impl)

(defun ensure-dense-array (array-like)
  (typecase array-like
    (array array-like)
    (t (asarray (ensure-list array-like)))))

;; FIXME: This should be in DENSE-ARRAYS itself?
(define-condition incompatible-broadcast-dimensions (error)
  ((dimensions :initarg :dimensions :reader condition-dimensions)
   (array-likes :initarg :array-likes :reader condition-array-likes))
  (:report (lambda (c s)
             (pprint-logical-block (s nil)
               (format s "The following array-likes with dimensions~%~{~%  ~S~}~%cannot be broadcast together:" (condition-dimensions c))
               (pprint-logical-block (s nil :per-line-prefix "  ")
                 (format s "~S" (condition-array-likes c)))))))

;; TODO: Add compiler-macros for this

(defun normalize-arguments/dmas (array-likes out)
  (if (every #'numberp array-likes)
      (values array-likes out)
      (let ((arrays (mapcar #'ensure-dense-array array-likes)))
        (multiple-value-bind (broadcast-compatible-p dimensions)
            (apply #'broadcast-compatible-p arrays)
          (if broadcast-compatible-p
              (values (apply #'broadcast-arrays arrays) (zeros dimensions))
              (error 'incompatible-broadcast-dimensions
                     :array-likes array-likes
                     :dimensions (mapcar #'dimensions array-likes)))))))

(macrolet ((def (name reduce-fn)
             `(define-splice-list-fn ,name (array-likes &key out)
                ;; TODO: Incorporate WHERE (?)
                (multiple-value-bind (array-likes out)
                    (normalize-arguments/dmas array-likes out)
                  (reduce (lambda (old-out new-value)
                            (,reduce-fn old-out new-value :out out))
                          (rest array-likes)
                          :initial-value (first array-likes))))))
  (def dn:+ dn:two-arg-+)
  (def dn:- dn:two-arg--))

(macrolet ((def (name reduce-fn invert-fn)
             `(define-splice-list-fn ,name (array-likes &key out)
                ;; TODO: Incorporate WHERE (?)
                (multiple-value-bind (array-likes out)
                    (normalize-arguments/dmas array-likes out)
                  (if (rest array-likes)
                      (reduce (lambda (old-out new-value)
                                (,reduce-fn old-out new-value :out out))
                              (rest array-likes)
                              :initial-value (first array-likes))
                      (,invert-fn (first array-likes)))))))
  (def dn:- dn:two-arg-- dn:one-arg--)
  (def dn:/ dn:two-arg-- dn:one-arg-/))
