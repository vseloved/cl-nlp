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

(defmethod train ((model softmax) data &key verbose (max-steps 1000) batch-size
                                         (al 0.1) (l1 0) (l2 0.1) (de 1e-6))
  (sgd model data
       :verbose verbose :max-steps max-steps :batch-size batch-size
       :al al :l1 l1 :l2 l2 :de de))

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
