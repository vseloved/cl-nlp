;;; (c) 2014 Vsevolod Dyomkin

(in-package #:nlp.learning)
(named-readtables:in-readtable rutils-readtable)


(defclass perceptron (categorical-model)
  ((weights :initform #h() :accessor m-weights)))

(defclass avg-perceptron (perceptron)
  ((step :initform 0 :accessor ap-step)
   (timestamps :initform #h() :accessor ap-timestamps)
   (totals :initform #h() :accessor ap-totals)))

(defclass greedy-ap (avg-perceptron)
  ())

;; (defmethod init-model ((model greedy-ap))
;;   (setf (ap-step model) 0
;;         (m-weights model) (make-array 0 :adjustable t :fill-pointer t)
;;         (ap-totals model) (make-array 0 :adjustable t :fill-pointer t)
;;         (ap-timestamps model) (make-array 0 :adjustable t :fill-pointer t)))

(defmethod classify ((model greedy-ap) fs)
  (let ((weights (m-weights model))
        (scores #h())
        (max 0)
        argmax)
    (dolist (f fs)
;      (unless (>= f (length weights))
        (dotable (class weight (get# f weights #h()))
          (incf (get# class scores 0) weight)))
    (dotable (class score scores)
      (when (> score max)
        (setf max score
              argmax class)))
    (values argmax
            max)))

(defmethod train1 ((model avg-perceptron) fs gold guess)
  (incf (ap-step model))
  (dolist (f fs)
    (ensure-f-init model f gold guess)
    (loop
       :for c :in (list gold guess)
       :for v :in '(1 -1) :do
       (update1 model f c v))))

(defmethod update1 ((model avg-perceptron) f c v)
  (with-slots (step timestamps weights totals) model
    (incf (? totals f c)
          (* (- step (? timestamps f c))
             (? weights f c)))
    (setf (? timestamps f c) step)
    (incf (? weights f c) v)))

;; (defmethod ensure-f-init ((model avg-perceptron) f &rest cs)
;;   (with-slots (weights totals timestamps) model
;;     (loop :repeat (- f (length weights)) :do
;;        (vector-push-extend #h() weights)
;;        (vector-push-extend #h() totals)
;;        (vector-push-extend #h() timestamps))
;;     (dolist (c cs)
;;       (unless (? totals f c)
;;         (setf (? totals f c) 0
;;               (? timestamps f c) 0
;;               (? weights f c) 0)))))

(defmethod ensure-f-init ((model avg-perceptron) f &rest cs)
  (with-slots (timestamps weights totals) model
    (unless (get# f totals)
      (set# f totals #h())
      (set# f timestamps #h())
      (set# f weights #h()))
    (dolist (c cs)
      (unless (get# c (get# f totals))
        (set# c (get# f totals) 0)
        (set# c (get# f timestamps) 0)
        (set# c (get# f weights) 0)))))
