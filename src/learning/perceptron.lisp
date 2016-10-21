;;; (c) 2014-2016 Vsevolod Dyomkin

(in-package #:nlp.learning)
(named-readtables:in-readtable rutilsx-readtable)


(defclass perceptron (categorical-model)
  ())

(defclass avg-perceptron (perceptron)
  ((step :initform 0 :accessor ap-step)
   (timestamps :initform #h() :accessor ap-timestamps)
   (totals :initform #h() :accessor ap-totals)))

(defmethod score ((model perceptron) class fs)
  (let ((rez 0)
        (weights (get# class (m-weights model) #h())))
    (dolist (f fs)
      (:+ rez (get# f weights 0))) ;(1- (random 2.0)))))
    rez))

(defmethod rank ((model perceptron) fs &key classes)
  (let ((scores #h()))
    (dotable (class weights (m-weights model))
      (when (or (null classes)
                (member class classes))
        (dolist (f fs)
          (:+ (get# class scores 0) (get# f weights 0)))))
    scores))


;;; training

(defmacro training-perceptron ((sample data epochs verbose c n) &body body)
  "Common scaffold for training different perceptron models."
  (with-gensyms (j prev-j total epoch)
    `(dotimes (,epoch ,epochs)
       (let ((,c 0) (,n 0) (,prev-j 0) (,total (length ,data)))
         (when ,verbose
           (format *debug-io* "~%~%==== Epoch: ~A ====~%~%" (1+ ,epoch)))
         (doindex (,j ,sample ,data)
           ,@body
           (when (and ,verbose
                      (> (/ (- ,j ,prev-j) ,total) 0.01))
             (:= ,prev-j ,j)
             (format *debug-io* "~A / ~A = ~5F% - ~2F% ~%"
                     ,c ,n (float (* 100 (/ ,c ,n)))
                     (* 100 (/ ,j ,total))))
           (unless ,verbose (princ-progress ,j ,total)))
         (:= ,data (shuffle ,data))))))

(defmethod train ((model avg-perceptron) data &key (epochs 5) verbose)
  (training-perceptron (sample data epochs verbose c n)
    (with-pair (gold fs) sample
      (let ((guess (classify model fs)))
        (train1 model gold guess fs)
        (when verbose
          (:+ c (if (eql gold guess) 1 0))
          (:+ n)))))
  model)

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

(defmethod train1 ((model perceptron) gold guess gold-fs &optional guess-fs)
  (:+ (ap-step model))
  (loop :for class :in (list gold guess)
        :for val :in '(1 -1)
        :for fs :in (list gold-fs (or gold-fs guess-fs)) :do
    (dolist (f fs)
      (ensure-fs-init model f class)
      (update1 model f class val))))

(defmethod update1 ((model avg-perceptron) f class val)
  (with-slots (step timestamps weights totals) model
    (:+ (? totals class f) (* (- step (? timestamps class f))
                              (? weights class f)))
    (:+ (? weights class f) val)
    (:= (? timestamps class f) step)))

(defmethod ensure-fs-init ((model avg-perceptron) f &rest classes)
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
