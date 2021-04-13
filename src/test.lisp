(in-package :dense-numericals.impl)

(defmacro define-numericals-test (name array-type single-float-error double-float-error)

  (let ((cl-name (find-symbol (symbol-name name) :cl)))

    (flet ((verification-form (type error)
             `(progn
                (5am:is-true (let ((rand (rand 1000 :type ',type)))
                               (array= (macro-map-array ',cl-name rand)
                                       (,name rand)
                                       :test (lambda (x y)
                                               (< (/ (abs (- x y)) (abs x))
                                                  ,error)))))
                (5am:is-true (let* ((rand (aref (rand '(100 100) :type ',type)
                                                '(10 :step 2))))
                               (array= (macro-map-array ',cl-name rand)
                                       (,name rand :out rand)
                                       :test (lambda (x y)
                                               (if (not (< (/ (abs (- x y)) (abs x))
                                                           ,error))
                                                   (progn (print (list x y)) nil)
                                                   t)))))
                (5am:is-true (let ((rand (aref (rand '(100 100) :type ',type)
                                               '(10 :step 2)
                                               '(10 :step 2))))
                               (array= (macro-map-array ',cl-name rand)
                                       (,name rand :out rand)
                                       :test (lambda (x y)
                                               (< (/ (abs (- x y)) (abs x))
                                                  ,error)))))
                (5am:is-true (let ((rand (aref (rand '(100 100) :type ',type)
                                               nil
                                               '(10 :step -2))))
                               (array= (macro-map-array ',cl-name rand)
                                       (,name rand :out rand)
                                       :test (lambda (x y)
                                               (< (/ (abs (- x y)) (abs x))
                                                  ,error)))))
                (5am:is-true (let* ((array (asarray '((1 2 3) (4 5 6))
                                                    ',type)))
                               (,name (aref array nil 1)
                                       :out (aref array nil 1))
                               (equalp '(1 3 4 6)
                                       (list (aref array 0 0)
                                             (aref array 0 2)
                                             (aref array 1 0)
                                             (aref array 1 2))))))))

      `(5am:def-test ,name (:suite ,array-type)
         ,(verification-form 'single-float single-float-error)
         ,(verification-form 'double-float double-float-error)))))
