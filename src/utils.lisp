(in-package :dense-numericals.impl)

(defun ensure-appropriate-dense-array (array-like)
  (if (typep array-like `(array ,default-element-type))
      array-like
      (asarray (ensure-list array-like) default-element-type)))

(declaim (inline ptr))
(declaim (ftype (function * cffi-sys::foreign-pointer) ptr))

#+sbcl
(defun ptr (array &optional (elt-size 1 elt-size-p))
  (declare (optimize speed)
           (type size elt-size)
           (type array array))
  (cffi:make-pointer (the fixnum
                          (+ (the fixnum
                                  (static-vectors::static-vector-address
                                   (array-displaced-to array)))
                             static-vectors::+array-header-size+
                             (the-size
                              (if elt-size-p
                                  (* elt-size
                                     (the-size
                                      (loop :for o :of-type size :in (array-offsets array)
                                            :with sum :of-type size := 0
                                            :do (incf sum o)
                                            :finally (return sum))))
                                  0))))))

#-sbcl
(defun ptr (array &optional (elt-size 1 elt-size-p))
  (declare (optimize speed)
           (type array array))
  (cffi:inc-pointer (static-vectors:static-vector-pointer (array-storage array))
                    (the-size (if elt-size-p
                                  (* elt-size
                                     (the-size
                                      (loop :for o :of-type size :in (array-offsets array)
                                            :with sum :of-type size := 0
                                            :do (incf sum o)
                                            :finally (return sum))))
                                  0))))



