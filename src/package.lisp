(cl:in-package :cl)

(defpackage :dense-numericals-lite
  (:documentation "Functionality in this package is available with pure lisp.")
  (:use)
  (:export +))

(uiop:define-package :dense-numericals
  (:use)
  (:documentation "Depends on foreign-functions")
  (:mix :dense-numericals-lite)
  (:reexport :dense-numericals-lite)
  (:export sin))

(uiop:define-package :dense-numericals.impl
  (:mix :dense-arrays-plus-lite :cl :alexandria)  
  (:import-from :adhoc-polymorphic-functions
                :define-polymorphic-function
                :defpolymorph)
  (:import-from :dense-arrays
                :lm
                :array-strides
                :array-offsets
                :size
                :the-size
                :int-index
                :the-int-index))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (unless (find-package :dense-numericals.c)
    (defpackage :dense-numericals.c
      (:use)
      (:export :*src-dir*))))

(in-package :dense-numericals.c)
(cl:defvar *src-dir* (asdf:component-pathname (asdf:find-system "dense-numericals")))

(cl:in-package :dense-numericals.impl)

(loop :for (nick package) :in '((:dn :dense-numericals)
                                  (:c  :dense-numericals.c))
      :do (trivial-package-local-nicknames:add-package-local-nickname nick package))

(5am:def-suite :dense-numericals)
