(in-package :dense-numericals.impl)

(defmacro define-numericals-test (name array-type
                                  (single-float-error
                                   &optional (single-float-min 0.0f0) (single-float-max 1.0f0))
                                  (double-float-error
                                   &optional (double-float-min 0.0d0) (double-float-max 1.0d0)))

  (let ((cl-name (find-symbol (symbol-name name) :cl)))

    (flet ((verification-form (type error min max)
             `(progn
                (flet ((float-close-p (x y)
                         (< (/ (abs (- x y)) (abs x))
                            ,error)))
                  (5am:is-true (let ((rand (rand 1000 :type ',type :min ,min :max ,max)))
                                 (array= (macro-map-array ',cl-name rand)
                                         (,name rand)
                                         :test #'float-close-p)))
                  (5am:is-true (let* ((rand (aref (rand '(100 100) :type ',type
                                                                   :min ,min :max ,max)
                                                  '(10 :step 2))))
                                 (array= (macro-map-array ',cl-name rand)
                                         (,name rand :out rand)
                                         :test #'float-close-p)))
                  (5am:is-true (let ((rand (aref (rand '(100 100) :type ',type
                                                                  :min ,min :max ,max)
                                                 '(10 :step 2)
                                                 '(10 :step 2))))
                                 (array= (macro-map-array ',cl-name rand)
                                         (,name rand :out rand)
                                         :test #'float-close-p)))
                  (5am:is-true (let ((rand (aref (rand '(100 100) :type ',type
                                                                  :min ,min :max ,max)
                                                 nil
                                                 '(10 :step -2))))
                                 (array= (macro-map-array ',cl-name rand)
                                         (,name rand :out rand)
                                         :test #'float-close-p)))
                  (5am:is-true (let* ((array (rand '(2 3) :type ',type
                                                          :min ,min :max ,max))
                                      (orig  (list (aref array 0 0)
                                                   (aref array 0 2)
                                                   (aref array 1 0)
                                                   (aref array 1 2))))
                                 (,name (aref array nil 1)
                                        :out (aref array nil 1))
                                 (equalp orig
                                         (list (aref array 0 0)
                                               (aref array 0 2)
                                               (aref array 1 0)
                                               (aref array 1 2)))))))))

      `(5am:def-test ,name (:suite ,array-type)
         ,(verification-form 'single-float single-float-error
                             single-float-min single-float-max)
         ,(verification-form 'double-float double-float-error
                             double-float-min double-float-max)))))
