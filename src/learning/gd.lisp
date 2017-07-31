;;; (c) 2017 Vsevolod Dyomkin

(in-package #:nlp.learning)
(named-readtables:in-readtable rutilsx-readtable)


(defun sgd (model data &key verbose (max-steps 1000) batch-size
                         (al 0.1) (l1 0) (l2 0.1) (de 1e-6))
  "Stochastic batch gradient descent for MODEL's weights on DATA
   with BATCH-SIZE (if not supplied - on the whole ata set,
   an integer is taken as is, and a float/ratio is treated as a ratio).
   AL is alfa (learning rate), L1 & L2 are the appropriate regularizations,
   DE (delta) and MAX-STEPS regulate exit conditions."
  (with ((batch-size (typecase batch-size
                       (integer batch-size)
                       ((or float ratio) (floor (* (length data) batch-size)))))
         (n (or batch-size (length data)))
         (fs-size (length (first (vals @model.weights)))))
    (dotimes (i max-steps)
      (with ((batch (if batch-size (sample data batch-size) data))
             (grad #h())
             (prev-cost (cost model batch :l1 l1 :l2 l2))
             (cc 0)
             (tt 0))
        (dotable (class _ @model.weights)
          (:= (? grad class) (make-array fs-size)))
        (dolist (ex batch)
          (let ((guess (rank model @ex.fs)))
            (when verbose
              (:+ cc (if (eql @ex.gold (keymax guess)) 1 0))
              (:+ tt))
            (grad1 model grad guess ex)))
        (dotable (class ws @model.weights)
          (let ((gd (? grad class)))
            (dotimes (i fs-size)
              (:- (? ws i)
                  (* al
                     (+ (/ (? gd i) n)
                        l1
                        (* l2 (? ws i))))))))
        (with ((new-cost (cost model batch :l1 l1 :l2 l2))
               (delta (abs (- new-cost prev-cost))))
          (when verbose
            (format *debug-io*
                    "Step ~A cost = ~5F (delta = ~5F) (accuracy: ~5F%)~%"
                    i new-cost delta (float (* 100 (/ cc tt)))))
          (when (< delta de)
            (return))))))
  model)

(defgeneric grad1 (model grad guess ex)
  (:documentation
   "Caclulate the current value of the model's gradient GRAD using
    GUESSed and EX examples."))
