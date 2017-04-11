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
   "Ensure that all the necessary tables are stup for feature F and classes CS
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
