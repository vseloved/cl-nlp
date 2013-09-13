;;; (c) 2013 Vsevolod Dyomkin

(in-package #:nlp.core)
(named-readtables:in-readtable rutils-readtable)


(defgeneric train (model data &rest args &key)
  (:documentation
   "Train some MODEL with the provided DATA."))

(defgeneric normalize (form data)
  (:documentation
   "Normalize DATA to some FORM."))

(defgeneric denormalize (form data)
  (:documentation
   "Denormalize DATA from some FORM."))