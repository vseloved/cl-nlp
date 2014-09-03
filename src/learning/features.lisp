;;; (c) 2014 Vsevolod Dyomkin

(in-package #:nlp.learning)
(named-readtables:in-readtable rutils-readtable)


(defgeneric extract-fs (model &rest args)
  (:documentation
   "Extract features for MODEL from ARGS."))

(defgeneric extract-gold (model data)
  (:documentation
   "Extract from data a list of gold values and corresponding features
    for MODEL."))

(defgeneric ensure-f-init (model f &rest cs)
  (:documentation
   "Ensure that all the necessary tables are stup for feature F and classes CS
    in the MODEL."))


;;; utils

(defmacro make-fs (model &rest fs-templates)
  "Return a list of MODEL-specific feature indices for FS-TEMPLATES."
  `(list
     ,@(mapcar (lambda (x)
                 `(intern ,(if (listp x)
                               `(strcat ,(first x) " " ,@(rest x))
                               x)
                          :f))
               fs-templates)))

(defun fs-idx (f fs)
  "Return an index for feature F in the index table FS or add it to the index."
  (or (get# f fs)
      (set# f fs (ht-count fs))))
