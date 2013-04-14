;;; (c) 2013 Vsevolod Dyomkin

(in-package #:nlp.corpora)
(named-readtables:in-readtable rutils-readtable)


(defun attr (name attributes)
  "Shortut XML attribute accessor."
  (sax::standard-attribute-value
   (find name attributes :test 'string=
         :key #'sax::standard-attribute-local-name)))
