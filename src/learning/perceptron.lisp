;;; (c) 2014 Vsevolod Dyomkin

(in-package #:nlp.learning)
(named-readtables:in-readtable rutils-readtable)


(defclass perceptron (categorical-model)
  ())

(defclass avg-perceptron (perceptron)
  ((step :initform 0 :accessor ap-step)
   (timestamps :initform #h() :accessor ap-timestamps)
   (totals :initform #h() :accessor ap-totals)))

(defmethod rank ((model perceptron) fs)
  (let ((scores #h()))
    (dotable (class weights (m-weights model))
      (dolist (f fs)
        (:+ (get# class scores 0) (get# f weights 0))))
    scores))

(defmethod train ((model avg-perceptron) data &key (epochs 5) verbose)
  (dotimes (epoch epochs)
    (when verbose
      (format t "~%~%==== Epoch: ~A ====~%~%" (1+ epoch)))
    (let ((total (length data)) (prev-j 0) (c 0) (n 0))
      (doindex (j sample data)
        (ds-bind (gold . fs) sample
          (let ((guess (classify model fs)))
            (train1 model fs gold guess)
            (when verbose
              (:+ c (if (eql gold guess) 1 0))
              (:+ n))))
        (when (and verbose
                   (> (/ (- j prev-j) total) 0.01))
          (setf prev-j j)
          (format t "~A / ~A = ~5F% - ~2F% ~%"
                  c n (float (* 100 (/ c n)))
                  (* 100 (/ j total))))
        (unless verbose (princ-progress j total))))))

(defmethod train :after ((model avg-perceptron) data &key)
  (with-slots (step totals weights timestamps) model
    (dotable (class cur-weights weights)
      (dotable (f weight cur-weights)
        (update1 model f class 0)  ; final update of totals
        (if (zerop weight)
            (progn
              (rem# f cur-weights)
              (rem# f (? totals class))
              (rem# f (? timestamps class)))
            (set# f cur-weights (/ (? totals class f) step)))))))

(defmethod train1 ((model perceptron) fs gold guess)
  (:+ (ap-step model))
  (dolist (f fs)
    (ensure-f-init model f gold guess)
    (loop
       :for class :in (list gold guess)
       :for val :in '(1 -1) :do
       (update1 model f class val))))

(defmethod update1 ((model avg-perceptron) f class val)
  (with-slots (step timestamps weights totals) model
    (:+ (? totals class f) (* (- step (? timestamps class f))
                              (? weights class f)))
    (:+ (? weights class f) val)
    (:= (? timestamps class f) step)))

(defmethod ensure-f-init ((model avg-perceptron) f &rest classes)
  (with-slots (timestamps weights totals) model
    (dolist (class classes)
      (unless (? weights class)
        (:= (? totals class) #h(equal)
            (? timestamps class) #h(equal)
            (? weights class) #h(equal)))
      (unless (? weights class f)
        (:= (? totals class f) 0
            (? timestamps class f) 0
            (? weights class f) 0)))))
