;;; (c) 2015-2016 Vsevolod Dyomkin

(in-package #:nlp.learning)
(named-readtables:in-readtable rutilsx-readtable)


(defclass random-forest ()
  ((trees :accessor forest-trees :initform () :initarg :trees)))


(defmethod train ((model random-forest) data
                  &key (n 10) (min-size 5) verbose)
  (let ((train-len (* 2/3 (length data)))
        (train-rank (/ (sqrt (length (first (lt data)))) 2)))  ;; also possible 1/2x & 2x
    (dotimes (i n)
      (let ((dt (make 'cart-tree)))
        (train dt (sample data train-len)
               :min-size min-size :idx-count train-rank :verbose verbose)
        (when verbose (format *debug-io* "."))
        (push dt (forest-trees model)))))
  (when verbose (terpri *debug-io*))
  model)

(defmethod rank ((model random-forest) fs &key classes)
  (let* ((trees (forest-trees model))
         (w (/ 1 (length trees)))
         (rez (mapcar #`(classify % fs :classes classes)
                      trees))
         (scores #h()))
    (dolist (class rez)
      (:+ (get# class scores 0) w))
    scores))
