(in-package :dense-numericals.impl)

(defun ensure-appropriate-dense-array (array-like)
  (if (typep array-like `(array ,default-element-type))
      array-like
      (asarray (ensure-list array-like) default-element-type)))
