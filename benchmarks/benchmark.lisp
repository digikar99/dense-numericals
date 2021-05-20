(in-package :dense-numericals/benchmarks)

(defstruct fun-report
  (name)
  (array-sizes)
  (lisp)
  (numpy)
  (torch))

(defstruct report
  (element-type)
  (fun-reports))

(defvar *report*)

(defun report (report &optional (stream *standard-output*))
  (let ((s stream)
        (ascii-table:*default-value-formatter*
          (lambda (value)
            (typecase value
              (float (format nil "~,2fx" value))
              (symbol (format nil "~S" value))
              (t (format nil "~A" value))))))
    (format s "ELEMENT-TYPE: ~A~%" (string-downcase (report-element-type report)))
    (dotimes (i 80) (write-char #\= s))
    (terpri s)
    (loop :for fun-report :in (reverse (report-fun-reports report))
          :do (with-slots (name array-sizes lisp numpy torch) fun-report
                (let* ((num-cols (apply #'min (mapcar #'length
                                                      (list array-sizes
                                                            lisp
                                                            numpy
                                                            torch))))
                       (array-sizes (subseq array-sizes 0 num-cols))
                       (lisp  (subseq lisp 0 num-cols))
                       (numpy (subseq numpy 0 num-cols))
                       (torch (subseq torch 0 num-cols)))
                  (ascii-table:display
                   (let ((table (ascii-table:make-table (cons "Library"
                                                              (mapcar #'write-to-string
                                                                      array-sizes))
                                                        :header (string name))))
                     (ascii-table:add-row table
                                          (cons 'numpy
                                                (mapcar #'/ numpy lisp)))
                     (when torch
		               (ascii-table:add-row table
                                            (cons 'torch
                                                  (mapcar #'/ torch lisp))))
                     table)
                   s))))))

(defun numpy-element-type (lisp-element-type)
  (ecase lisp-element-type
    (single-float 'np.float32)
    (double-float 'np.float64)))

(defun torch-element-type (lisp-element-type)
  (ecase lisp-element-type
    (single-float 't.float32)
    (double-float 't.float64)))

(defmacro time-it (&body body)
  (with-gensyms (start end body-result)
    `(let (,start ,end ,body-result)
       (setq ,start (/ (get-internal-real-time)
                       1.0 internal-time-units-per-second))
       (setq ,body-result (progn ,@body))
       (setq ,end (/ (get-internal-real-time)
                     1.0 internal-time-units-per-second))
       (values (- ,end ,start) ,body-result))))

;;; May need to run: (cffi:foreign-funcall "fedisableexcept" :int -1)
;;; Reference:
;;; - https://stackoverflow.com/questions/19363484/representing-infinity-and-nan-independent-of-implementation
;;; - https://linux.die.net/man/3/fedisableexcept

(defparameter *numpy* t)
(defparameter *torch* #+arm64 nil #-arm64 t)

(defun benchmark (&rest element-types)
  (let (reports)
    (unwind-protect
         (loop :for element-type :in element-types :do
           (let ((*array-element-type* element-type)
                 (*report* (make-report :element-type element-type)))
             (push *report* reports)

             (one-arg-fn '(dn:sin dn:cos dn:tan)
                         '(np.sin np.cos np.tan)
                         '( t.sin  t.cos  t.tan))

             (one-arg-fn '(dn:sinh dn:cosh dn:tanh)
                         '(np.sinh np.cosh np.tanh)
                         '( t.sinh  t.cosh  t.tanh))

             (one-arg-fn '(dn:asin   dn:acos   dn:atan)
                         '(np.arcsin np.arccos np.arctan)
                         '( t.arcsin  t.arccos  t.arctan))

             (one-arg-fn '(dn:asinh   dn:acosh   dn:atanh)
                         '(np.arcsinh np.arccosh np.arctanh)
                         '( t.arcsinh  t.arccosh  t.arctanh))

             ;; (two-arg-fn '(dn:expt  dn:atan)
             ;;             '(np.power np.arctan2)
             ;;             '( t.pow    t.atan2))

             ;; (two-arg-fn '(dn:atan)
             ;;             '(np.arctan2)
             ;;             '(t.atan2))
             (two-arg-fn '(dn:expt)
                         '(np.power)
                         '( t.pow))
             ))
      (return-from benchmark (values-list reports)))))
