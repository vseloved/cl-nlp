;;; (c) 2014-2017 Vsevolod Dyomkin

(in-package #:nlp.learning)
(named-readtables:in-readtable rutilsx-readtable)


(defgeneric extract-fs (model &rest args)
  (:documentation
   "Extract features for MODEL from ARGS."))

(defgeneric extract-gold (model data)
  (:documentation
   "Extract from data a list of pairs of gold values
    and corresponding features for MODEL."))

(defgeneric ensure-fs-init (model f &rest cs)
  (:documentation
   "Ensure that all the necessary tables are set up for feature F and classes CS
    in the MODEL."))


;;; utils

(defvar *strpool* #h(equal)
        "String pool for features.")

(defmacro make-fs (&rest fs-templates)
  "Return a list features based on FS-TEMPLATES."
  `(list
     ,@(mapcar (lambda (tmpl)
                 (let ((str (etypecase tmpl
                              (list `(strcat ,@(rest (flat-map ^(list " " %)
                                                               tmpl))))
                              (string tmpl))))
                   (getset# str *strpool* str)))
               fs-templates)))

(defun f-by-id (model id)
  "Get feature name by ID from *fmap*."
  (dotable (k v @model.fs-dict)
    (when (= v id) (return k))))

(defun princ-fs (model fs)
  "Princ on newlines names of features in the sequence FS."
  (dolist (f fs)
    (format nil "~A (~A)~%" (f-by-id model f) f)))

(defun f-weight (model f class)
  "Get weight of feature F for CLASS in the classifier MODEL."
  (? (? model 'weights class)
     (? model 'fs-dict f)))

(defun fs-vec (model fs)
  "Convert a list of raw features FS to the vector
   of global feature ids (numbers) that are also reflected
   in the MODEL's fs-dict."
  (let ((dict @model.fs-dict))
    (uniq (map 'vector ^(getset# % dict (ht-count dict))
               fs))))
