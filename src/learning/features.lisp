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

(defmacro make-fs (&rest fs-templates)
  "Return a list features based on FS-TEMPLATES."
  `(list
     ,@(mapcar (lambda (tmpl)
                 (etypecase tmpl
                   (list `(strcat ,@(rest (mappend #`(list " " %) tmpl))))
                   (string tmpl)))
               fs-templates)))

(defun fs-idx (f fs)
  "Return an index for feature F in the index table FS or add it to the index."
  (or (get# f fs)
      (set# f fs (ht-count fs))))
