(uiop:define-package :dense-numericals/examples.mnist
  (:mix :trivial-coerce :dense-numericals :dense-arrays-plus-lite
   :iterate :cl :alexandria))

(in-package :dense-numericals/examples.mnist)

(push (cons (find-package :dense-numericals/examples.mnist)
            'double-float)
      *array-element-type-alist*)

(defun load-data (x-file-name y-file-name)
  (list (numpy-file-format:load-array x-file-name)
        (numpy-file-format:load-array y-file-name)))

(defun load-data-as-dense-arrays (x-file-name y-file-name &optional (show-progress t))
  ;; Takes about 20 sec on i7-8750h @ 3GHz
  (when show-progress (format t "Loading CL arrays..."))
  (let (dense-x dense-y)
    (destructuring-bind (x y) (load-data x-file-name y-file-name)
      (when show-progress (format t "Loaded!~%"))
      (when show-progress (format t "Converting x to dense-array..."))
      (setq dense-x (asarray x 'double-float))
      (when show-progress (format t "Converted!~%"))
      (when show-progress (format t "Converting y to dense-array..."))
      (setq dense-y (asarray y 'double-float))
      (when show-progress (format t "Converted!~%")))
    (list dense-x dense-y)))

(defun one-hot-encode (vector num-classes)
  ;; (declare (type (array cl:* 1) vector))
  (let ((zeros (zeros (shape vector 0) num-classes)))
    (loop :for idx :below (shape vector 0)
          :for col := (aref vector idx)
          :do (funcall #'(setf aref)
                       1.0d0
                       zeros
                       idx
                       (coerce col 'integer)))
    zeros))

(defun my-min (array)
  (let ((min-so-far most-positive-single-float))
    (do-arrays ((a array double-float))
      (when (cl:< a min-so-far)
        (setq min-so-far a)))
    min-so-far))

(defun my-max (array)
  (let ((max-so-far most-negative-single-float))
    (do-arrays ((a array double-float))
      (when (cl:> a max-so-far)
        (setq max-so-far a)))
    max-so-far))

(defun my-argmax (array)
  (declare (optimize speed)
           (type (array double-float 1) array))
  ;; numpy is 4 times faster than this :/
  (let ((max-so-far most-negative-double-float)
        (max-index  -1)
        (index      0))
    (declare (type fixnum index))
    (do-arrays ((a array double-float))
      (when (cl:> a max-so-far)
        (setq max-so-far a)
        (setq max-index  index))
      (incf index))
    max-index))

;;; Loading train data:
;; (let ((data 
;;         (time 
;;          (load-data-as-dense-arrays "~/ram-disk/train_x.npy" "~/ram-disk/train_y.npy"))))
;;   (defparameter *train-x* (let ((ir (concat (ones (shape (first data) 0) 1)
;;                                             (first data)
;;                                             :axis 1)))
;;                             (two-arg-/ ir 255.0d0 :out ir)
;;                             (two-arg-- ir (cl:/ (cl:+ (my-max ir)
;;                                                       (my-min ir))
;;                                                 2)
;;                                        :out ir)))
;;   (defparameter *train-y* (one-hot-encode (second data) 10)))

;;; Loading test data:
;; (let ((data 
;;         (time 
;;          (load-data-as-dense-arrays "~/ram-disk/test_x.npy" "~/ram-disk/test_y.npy"))))
;;   (defparameter *test-x* (let ((ir (concat (ones (shape (first data) 0) 1)
;;                                            (first data)
;;                                            :axis 1)))
;;                            (two-arg-/ ir 255.0d0 :out ir)
;;                            (two-arg-- ir (cl:/ (cl:+ (my-max ir)
;;                                                      (my-min ir))
;;                                                2)
;;                                       :out ir)))
;;   (defparameter *test-y* (one-hot-encode (second data) 10)))

;; (defparameter *train-x* (concat (ones (shape *train-x* 0) 1)
;;                                 *train-x* :axis 1))
;; (defparameter *train-y* (one-hot-encode *train-y* 10))

(defun softmax! (x)
  ;; TODO: Optimize and inline
  (declare (type (array double-float 2) x))
  (let ((sum-x (zeros (shape x 0))))
    ;; (declare (dynamic-extent sum-x))
    (exp x :out x)
    (/ x
       (reshape (sum x :out sum-x :axes 1)
                (list (shape x 0) 1))
       :out x)))

(defun cross-entropy! (y-predicted y-true)
  (do-arrays ((yp y-predicted double-float)
              (yt y-true      double-float))
    (if (cl:= yt 1)
        (setf yp (- (log yp)))
        (setf yp 0.0d0)))
  y-predicted)

(defparameter *learning-rate* 0.01d0)

(defun predict (weights train-x &optional x-idx)
  ;; AREF is array-indexing operator/function
  ;; TRAIN-X is 60000x785
  ;; X is 1x785
  ;; WEIGHTS is 785x10
  (let ((x (aref train-x (if x-idx
                             `(,x-idx :end ,(1+ x-idx))
                             nil))))
    ;; (print (list ;; x
    ;;         (shape x)
    ;;         (shape weights)
    ;;         (sum (copy x))))
    (softmax! (two-arg-matmul (copy x)
                              weights
                              :out (zeros (shape x 0)
                                          (shape weights 1))))))

(defun accuracy (weights x y)
  (let ((predictions (predict weights x))
        (num-correct 0)
        (num-total   (shape y 0)))
    (loop :for i :below num-total
          :for pred := (aref predictions i)
          :for true := (aref y i)
          :do (let ((pred (my-argmax pred))
                    (true (my-argmax true)))
                (if (cl:= pred true)
                    (incf num-correct))))
    (values (cl:/ num-correct num-total 1.0d0)
            num-correct
            num-total)))

(defun fit (train-x train-y num-iterations
            &key (loss-interval (floor num-iterations 10))
              (batch-size 4)
              weights)
  (let* ((num-y-classes (shape train-y 1))
         (num-images    (shape train-x 0))
         (weights (or weights
                      (rand (shape train-x 1)
                            num-y-classes)))
         (grad    (zeros-like weights))
         (y-predicted (zeros batch-size num-y-classes))
         (softmax-y-holder (zeros-like y-predicted))
         (cross-entropy-y-holder (zeros-like y-predicted))
         (x-holder    (zeros batch-size (shape train-x 1)))
         (x-holder^t  (zeros-like (transpose x-holder))))
    ;; (print (list (shape x-holder)
    ;;              (shape weights)
    ;;              (shape y-predicted)))
    ;; (break)
    
    (unwind-protect
         
         (iter outer

           (for iter below num-iterations)

           (for loss = 0)
           (for p-loss previous loss)
           
           (iter (for img-idx below num-images by batch-size)

             ;; Feedforward
             (two-arg-matmul (copy (aref train-x (list img-idx
                                                       :end (cl:+ img-idx batch-size)))
                                   :out x-holder)
                             weights
                             :out y-predicted)
             (softmax! (copy y-predicted :out softmax-y-holder))

             (for y-true = (aref train-y (list img-idx
                                               :end (cl:+ img-idx batch-size))))

             ;; Update loss
             (incf loss (sum (cross-entropy! (copy softmax-y-holder
                                                   :out cross-entropy-y-holder)
                                             y-true)))

             ;; Back-propagate
             ;; (if (or (in outer (first-iteration-p))
             ;;         (cl:< loss p-loss))
             ;;     (two-arg-+ weights
             ;;                (two-arg-* (cl:/ *learning-rate* num-images)
             ;;                           (two-arg-matmul (copy (transpose x-holder)
             ;;                                                 :out x-holder^t)
             ;;                                           (two-arg-- y-true
             ;;                                                      softmax-y-holder
             ;;                                                      :out softmax-y-holder)
             ;;                                           :out grad))
             ;;                :out weights)
             ;;     (progn
             ;;       (setq *learning-rate* (print (cl:/ *learning-rate* 2)))
             ;;       (setq loss p-loss)
             ;;       (terminate)))

             (two-arg-+ weights
                            (two-arg-* (cl:/ *learning-rate* num-images)
                                       (two-arg-matmul (copy (transpose x-holder)
                                                             :out x-holder^t)
                                                       (two-arg-- y-true
                                                                  softmax-y-holder
                                                                  :out softmax-y-holder)
                                                       :out grad))
                            :out weights))

           

           ;; Inform Loss           
           (when (zerop (rem iter loss-interval))
             (print loss)))
      
      (return-from fit weights))))
