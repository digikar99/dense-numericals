(in-package :dense-numericals.impl)

(defun ptr (array)
  (static-vectors:static-vector-pointer (array-displaced-to array)))

(defmacro ptr-iterate-but-inner (n-var bindings expression)
  "Each bindings is of the form (PTR-VAR TOTAL-INITIAL-OFFSET-VAR INNER-STRIDE-VAR ARRAY-EXPR)."

  (let* ((pointers      (mapcar #'first bindings))
         (array-exprs   (mapcar #'fourth bindings))
         (total-offsets (mapcar #'second bindings))
         (inner-strides (mapcar #'third bindings))
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
                         offsets array-vars)
               ,@(mapcar (lm total-offset `(,total-offset 0))
                         total-offsets))
           (declare (type int-index ,@total-offsets))

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
                              ,@(mapcar (lm total-offset o `(incf ,total-offset ,o))
                                        total-offsets os)
                              ,expression
                              ,@(mapcar (lm total-offset o `(decf ,total-offset ,o))
                                        total-offsets os))
                            (loop :initially
                                  ,@(mapcar (lm total-offset o `(incf ,total-offset ,o))
                                            total-offsets os)
                                  :repeat ,n-var
                                  :do (nest-loop
                                       (rest ,dimensions)
                                       ,@(mapcar (lm strides `(rest ,strides)) strides)
                                       ,@(mapcar (lm offsets `(rest ,offsets)) offsets))
                                  ,@(mapcar (lm total-offset ss `(incf ,total-offset ,ss))
                                            total-offsets ss)
                                  :finally
                                  ,@(mapcar (lm total-offset o s
                                                `(decf ,total-offset
                                                     (the int-index
                                                          (+ ,o
                                                             (the-int-index
                                                              (* ,n-var ,s))))))
                                            total-offsets os ss))))))
             (nest-loop (narray-dimensions ,(first array-vars))
                        ,@strides ,@offsets)))))))
