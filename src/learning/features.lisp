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


(defvar *fs-idx* #h()
  "Feature to index mapping for each model.")

(defmacro make-fs (model &rest fs-templates)
  "Return a list of MODEL-specific feature indices for FS-TEMPLATES."
  (with-gensyms (fs-idx)
    `(let ((,fs-idx (or (get# ,model *fs-idx*)
                        (set# ,model *fs-idx* #h()))))
       (mapcar #`(or (get# % ,fs-idx)
                     (set# % ,fs-idx (ht-count ,fs-idx)))
               (list ,@(mapcar (lambda (x) `(mkeyw (strcat ,@(mklist x))))
                               fs-templates))))))
