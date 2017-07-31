;;; (c) 2017 Vsevolod Dyomkin

(in-package #:nlp.learning)
(named-readtables:in-readtable rutilsx-readtable)


(defclass softmax (categorical-model)
  ((fs-dict :initarg :fs-dict :initform #h(equal) :accessor model-fs-dict)))

(defmethod score ((model softmax) class fs)
  (? (rank model fs) class))
  
(defmethod rank ((model softmax) fs &key classes)
  (let ((scores #h())
        (total 0))
    (dotable (class ws @model.weights)
      (when (or (null classes)
                (member class classes))
        (:+ total (:= (? scores class) (exp (sum '* ws fs))))))
    (dotable (class _ scores)
      (:/ (? scores class) total))
    scores))

;; initialize fs dict and weights
(defun make-subj-softmax (data)
  (let ((model (make 'softmax))
        (i -1))
    (dolist (f (sort (uniq (flat-map 'ex-fs data)) 'string<))
      (:= (? @model.fs-dict f) (:+ i)))
    (:+ i)
    (dolist (class (uniq (map* 'ex-gold data)))
      (:= (? @model.weights class)
          (make-array i :initial-contents
                      (maptimes i ^(* 0.01 (normal-random))))))
    model))

(defun dense->sparse (model ex)
  (let ((fs (make-array (ht-count @model.fs-dict))))
    (map nil ^(:= (? fs (? @model.fs-dict %)) 1.0)
         @ex.fs)
    (make-ex
     :fs fs
     :gold @ex.gold
     :raw @ex.raw)))


(defmethod train ((model softmax) data &key verbose (max-steps 1000) batch-size
                                         (al 0.1) (l1 0) (l2 0.1) (de 1e-6))
  (sgd model data
       :verbose verbose :max-steps max-steps :batch-size batch-size
       :al al :l1 l1 :l2 l2 :de de))

(defmethod sgd (model data &key verbose (max-steps 1000) batch-size
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
         (fs-size (ht-count @model.fs-dict)))
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

(defmethod grad1 ((model softmax) grad guess ex)
  (let ((fs-size (ht-count @model.fs-dict)))
    (dotable (class _ @model.weights)
      (let ((id (if (eql @ex.gold class) 1 0))
            (gd (? grad class))
            (p (? guess class)))
        (dotimes (i fs-size)
          (:+ (? gd i) (* (- p id) (? @ex.fs i)))))))
  grad)

(defmethod cost ((model softmax) data &key (l1 0) (l2 0.1))
  (let ((n (length data))
        (ws (apply 'concatenate 'vector (vals @model.weights))))
    (+ (- (/ (sum ^(log (score model @%.gold @%.fs))
                  data)
             n))
       (* l1 (sum 'abs ws))
       (* 1/2 l2 (sum ^(* % %) ws)))))

(defmethod save-model ((model softmax) out)
  (let ((total (sum 'length (vals @model.weights)))
        (i 0))
    (format out "~A~%" (ht-count @model.weights))
    (dotable (c fw @model.weights)
      (format out "~S~%" c)
      (format out "~{~A~^ ~}~%" (coerce fw 'list))
      (princ-progress (:+ i) total))
    (dolist (fs-idx (sort (ht->pairs @model.fs-dict) '< :key'rt))
      (format out "~A~%" (lt fs-idx)))))

(defmethod load-model ((model softmax) in &key class-package)
  (let ((total (read in))
        (i 0))
    (loop :repeat total :do
      (:= (? @model.weights (let ((*package* (or class-package *package*)))
                              (read in)))
          (map 'vector 'read-from-string (split #\Space (read-line in))))
      (princ-progress (:+ i) total))
    (loop :for i :from 0
          :for line := (read-line in nil)
          :while (and line (not (blankp line))) :do
      (:= (? @model.fs-dict line) i)))
  (rem# nil @model.weights)
  model)
