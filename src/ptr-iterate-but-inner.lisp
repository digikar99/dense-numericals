(in-package :dense-numericals.impl)

(defmacro ptr-iterate-but-inner (n-var &body (bindings . expression))
  "Each bindings is of the form (PTR-VAR ELT-SIZE INNER-STRIDE-VAR ARRAY-EXPR)."

  (let* ((pointers      (mapcar #'first  bindings))
         (elt-sizes     (mapcar #'second bindings))
         (inner-strides (mapcar #'third bindings))
         (array-exprs   (mapcar #'fourth  bindings))
         (num-arrays    (length bindings)))

    (let ((array-vars   (make-gensym-list num-arrays "ARRAY"))
          (offsets      (make-gensym-list num-arrays "OFFSETS"))
          (dimensions   (gensym "DIMENSIONS"))
          (ss           inner-strides) ; we use the same to avoid reassignment in nest-loop
          (os           (make-gensym-list num-arrays "OS"))
          (strides      (make-gensym-list num-arrays "STRIDES")))

      `(let (,@(mapcar (lm var expr `(,var ,expr))
                       array-vars array-exprs))
         (let (,@(mapcar (lm ptr expr `(,ptr (ptr ,expr)))
                         pointers array-vars)
               ,@(mapcar (lm strides var `(,strides (array-strides ,var)))
                         strides array-vars)
               ,@(mapcar (lm offsets var `(,offsets (array-offsets ,var)))
                         offsets array-vars))
           (declare (type cffi-sys:foreign-pointer ,@pointers))

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
                              ,@(mapcar (lm ptr o elt-size
                                            `(cffi:incf-pointer
                                                 ,ptr (the-int-index (* ,elt-size ,o))))
                                        pointers os elt-sizes)
                              ,@expression
                              ,@(mapcar (lm ptr o elt-size
                                            `(cffi:incf-pointer
                                                 ,ptr (the-int-index
                                                       (* ,elt-size
                                                          (- ,o)))))
                                        pointers os elt-sizes)
                              nil)
                            (loop :initially
                                  ,@(mapcar (lm ptr o elt-size
                                                `(cffi:incf-pointer
                                                     ,ptr (the-int-index (* ,elt-size ,o))))
                                            pointers os elt-sizes)
                                  :repeat ,n-var
                                  :do (nest-loop
                                       (rest ,dimensions)
                                       ,@(mapcar (lm strides `(rest ,strides)) strides)
                                       ,@(mapcar (lm offsets `(rest ,offsets)) offsets))
                                  ,@(mapcar (lm ptr s elt-size
                                                `(cffi:incf-pointer
                                                     ,ptr (the-int-index (* ,elt-size ,s))))
                                            pointers ss elt-sizes)
                                  :finally
                                  ,@(mapcar (lm ptr o s elt-size
                                                `(cffi:incf-pointer
                                                     ,ptr
                                                     (the-int-index
                                                          (* ,elt-size
                                                             (the-int-index
                                                                  (- (+ ,o
                                                                        (the-int-index
                                                                         (* ,n-var ,s)))))))))
                                            pointers os ss elt-sizes))))))
             (nest-loop (narray-dimensions ,(first array-vars))
                        ,@strides ,@offsets)))))))
