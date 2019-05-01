;;; (c) 2017 Vsevolod Dyomkin

(in-package #:nlp.learning)
(named-readtables:in-readtable rutilsx-readtable)


(defun sgd (model data &key verbose (epochs 1000) batch-size
                         (al 0.1) (mo 0.0)
                         (l1 0) (l2 0.1) (de 1e-6))
  "Stochastic batch gradient descent for MODEL's weights on DATA
   with BATCH-SIZE (if not supplied - on the whole ata set,
   an integer is taken as is, and a float/ratio is treated as a ratio).
   AL is alfa (learning rate), MO is momentum,
   L1 & L2 are the appropriate regularizations,
   DE (delta) and EPOCHS regulate exit conditions."
  (with ((batch-size (typecase batch-size
                       (integer batch-size)
                       ((or float ratio) (floor (* (length data) batch-size)))))
         (n (or batch-size (length data)))
         (fs-size (length (first (vals @model.weights)))))
    (flet ((init-grad ()
             (let ((g #h()))
               (dotable (class _ @model.weights)
                 (:= (? g class) (make-array fs-size)))
               g)))
      (let ((prev-grad (init-grad)))
        (dotimes (i epochs)
          (with ((batch (if batch-size (sample data batch-size) data))
                 (grad (init-grad))
                 (prev-cost (cost model batch :l1 l1 :l2 l2))
                 (cc 0)
                 (tt 0))
            (dovec (ex batch)
              (let ((guess (rank model @ex.fs)))
                (when verbose
                  (:+ cc (if (eql @ex.gold (keymax guess)) 1 0))
                  (:+ tt))
                (grad1 model grad guess ex)))
            (dotable (class ws @model.weights)
              (let ((gd (? grad class))
                    (prev (? prev-grad class)))
                (dotimes (i fs-size)
                  (:- (? ws i)
                      (+ (* al (+ (/ (? gd i) n)
                                   l1
                                   (* l2 (? ws i))))
                          (* mo (? prev i)))))))
            (:= prev-grad grad)
            (with ((cost (cost model batch :l1 l1 :l2 l2))
                   (delta (- prev-cost cost)))
              (when verbose
                (format *debug-io*
                        "Epoch ~A cost = ~5F (delta = ~5F) (accuracy: ~5F%)~%"
                        (1+ i) cost delta (float (* 100 (/ cc tt)))))
              (when (< (abs delta) de)
                (return))))))))
  model)

(defgeneric grad1 (model grad guess gold)
  (:documentation
   "Caclulate the current value of the model's gradient GRAD using
    GUESSed and GOLD examples."))
