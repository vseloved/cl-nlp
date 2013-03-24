;;; (c) 2013 Vsevolod Dyomkin

(in-package #:nlp.core)
(named-readtables:in-readtable rutils-readtable)
(declaim (optimize (compilation-speed 2) (speed 3) (space 2) (debug 1)))


(defgeneric train (model data &rest args &key)
  (:documentation
   "Train some model with the provided data."))