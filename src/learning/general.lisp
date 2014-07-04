;;; (c) 2014 Vsevolod Dyomkin

(in-package #:nlp.learning)
(named-readtables:in-readtable rutils-readtable)


(defclass categorical-model ()
  ((classes :initarg :classes :accessor m-classes)))


;; (defgeneric init-model (model)
;;   (:documentation
;;    "Initialize the MODEL."))

(defgeneric classify (model fs)
  (:documentation
   "Classify a feature set FS with a MODEL."))

(defgeneric train (model data &key)
  (:documentation
   "Train some MODEL with the provided DATA."))

(defgeneric train1 (model fs gold guess)
  (:documentation
   "Perform one step of MODEL training with features FS
    with GOLD and GUESS variants."))

(defgeneric evaluate (model gold-fs)
  (:documentation
   "Measure MODEL's performance on GOLD-FS pairs of gold result and featues.")
  (:method (model gold-fs)
    (let ((matched 0) (total 0) (len (length gold-fs)))
      (dolist (sample gold-fs)
        (with-pair (gold fs) sample
          (when (equal gold (classify model fs))
            (incf matched)))
        (princ-progress (incf total) len))
      (* 100.0 (/ matched total)))))
