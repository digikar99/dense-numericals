(uiop:define-package :dense-numericals.cuda
  (:mix :cl-cuda :adhoc-polymorphic-functions)
  (:import-from :dense-arrays :cuda-array))

(trivial-package-local-nicknames:add-package-local-nickname :dn :dense-numericals
                                                            :dense-numericals.cuda)

(trivial-package-local-nicknames:add-package-local-nickname :da :dense-arrays-plus-lite
                                                            :dense-numericals.cuda)

;; (uiop:define-package :dense-numericals+cuda
;;     (:reexport :dense-numericals))

(in-package :dense-numericals.impl)

(import '(dense-arrays::cuda-array dense-arrays::simple-cuda-array) :dense-numericals.impl)

(trivial-package-local-nicknames:add-package-local-nickname :dc :dense-numericals.cuda
                                                            :dense-numericals.impl)

;; TODO: Merge this with DENSE-NUMERICALS.IMPL::PTR-ITERATE-BUT-INNER
(defmacro device-ptr-iterate-but-inner (elt-size n-var bindings &body body)
  "Each bindings is of the form (DEVICE-PTR-VAR INNER-STRIDE-VAR ARRAY-EXPR)."

  (let* ((pointers      (mapcar #'first  bindings))
         (array-exprs   (mapcar #'third  bindings))
         (inner-strides (mapcar #'second bindings))
         (num-arrays    (length bindings)))

    (let ((array-vars   (make-gensym-list num-arrays "ARRAY"))
          (offsets      (make-gensym-list num-arrays "OFFSETS"))
          (dimensions   (gensym "DIMENSIONS"))
          (ss           inner-strides) ; we use the same to avoid reassignment in nest-loop
          (os           (make-gensym-list num-arrays "OS"))
          (strides      (make-gensym-list num-arrays "STRIDES")))

      `(let (,@(mapcar (lm var expr `(,var ,expr))
                       array-vars array-exprs))
         (declare (type dense-arrays::dense-array ,@array-vars))
         (let (,@(mapcar (lm ptr expr `(,ptr (cl-cuda:memory-block-device-ptr
                                              (array-storage ,expr))))
                         pointers array-vars)
               ,@(mapcar (lm strides var `(,strides (array-strides ,var)))
                         strides array-vars)
               ,@(mapcar (lm offsets var `(,offsets (array-offsets ,var)))
                         offsets array-vars))
           (declare (type fixnum ,@pointers))

           (labels ((nest-loop (,dimensions ,@strides ,@offsets)
                      (let ((,n-var   (first ,dimensions))
                            ,@(mapcar (lm ss strides `(,ss (first ,strides)))
                                      ss strides)
                            ,@(mapcar (lm os offsets `(,os (first ,offsets)))
                                      os offsets))
                        (declare (type int-index ,@os ,@ss)
                                 (type size ,n-var))
                        ;; (mapc #'print (list ,dimensions ,@pointers))
                        (if (null (rest ,dimensions))
                            (progn
                              ,@(mapcar (lm ptr o `(incf ,ptr
                                                         (the-int-index (* ,elt-size ,o))))
                                        pointers os)
                              ,@body
                              ,@(mapcar (lm ptr o `(incf ,ptr
                                                         (the-int-index (* ,elt-size (- ,o)))))
                                        pointers os)
                              nil)
                            (loop :initially
                                  ,@(mapcar (lm ptr o `(incf ,ptr
                                                             (the-int-index (* ,elt-size ,o))))
                                            pointers os)
                                  :repeat ,n-var
                                  :do (nest-loop
                                       (rest ,dimensions)
                                       ,@(mapcar (lm strides `(rest ,strides)) strides)
                                       ,@(mapcar (lm offsets `(rest ,offsets)) offsets))
                                  ,@(mapcar (lm ptr s `(incf ,ptr
                                                             (the-int-index (* ,elt-size ,s))))
                                            pointers ss)
                                  :finally
                                  ,@(mapcar (lm ptr o s
                                                `(incf ,ptr (the-int-index
                                                             (* ,elt-size
                                                                (the-int-index
                                                                 (- (+ ,o
                                                                       (the-int-index
                                                                        (* ,n-var ,s)))))))))
                                            pointers os ss))))))
             (nest-loop (narray-dimensions ,(first array-vars))
                        ,@strides ,@offsets)))))))
