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
  (:export #:sin
           #:cos
           #:tan
           #:asin
           #:acos
           #:atan
           #:sinh
           #:cosh
           #:tanh
           #:asinh
           #:acosh
           #:atanh

           #:exp
           #:log
           #:expt

           #:sqrt

           #:+
           #:two-arg-+
           #:-
           #:two-arg--
           #:/
           #:two-arg-/
           #:-
           #:two-arg-*

           #:<
           #:two-arg-<
           #:<=
           #:two-arg-<=
           #:=
           #:two-arg-=
           #:/=
           #:two-arg-/=
           #:>
           #:two-arg->
           #:>=
           #:two-arg->=))

(uiop:define-package :dense-numericals.impl
  (:mix :dense-arrays-plus-lite :cl :alexandria)
  (:import-from :adhoc-polymorphic-functions
                :define-polymorphic-function
                :defpolymorph
                :env
                :optim-speed
                :optim-debug
                :defpolymorph-compiler-macro)
  (:import-from :dense-arrays
                :lm
                :array-strides
                :array-offsets
                :size
                :the-size
                :int-index
                :the-int-index))

(in-package :dense-numericals.impl)

(defvar *src-dir* (asdf:component-pathname (asdf:find-system "dense-numericals")))

(unless (find-package :dense-numericals.c)
  (defpackage :dense-numericals.c
    (:use)
    (:import-from :dense-numericals.impl
                  :*src-dir*)
    (:export :*src-dir*)))

(unless (find-package :dense-numericals.linalg.c)
  (defpackage :dense-numericals.linalg.c
    (:use)
    (:import-from :dense-numericals.impl
                  :*src-dir*)
    (:export :*src-dir*)))

(loop :for (nick package) :in '((:dn       :dense-numericals)
                                (:c        :dense-numericals.c)
                                (:linalg.c :dense-numericals.linalg.c))
      :do (trivial-package-local-nicknames:add-package-local-nickname nick package))

(5am:def-suite :dense-numericals)

(push (cons (find-package :dense-numericals.impl) 'single-float)
      *element-type-alist*)
