;;; (c) 2014 Vsevolod Dyomkin

(in-package #:ntag)
(named-readtables:in-readtable rutils-readtable)


(defun extract-sents (text)
  (mapcar #`(make 'ncore:sentence :tokens (ncorp:remove-dummy-tokens %))
          (ncorp:text-tokens text)))

(defvar *tagger* (load-model (make 'greedy-ap-tagger)
                             (model-file "pos-tagging/wsj.zip")))

(defvar *gold*
  (let (test)
    (ncorp:map-corpus :treebank (corpus-file "onf-wsj/")
                      #`(appendf test (extract-sents %)))
    (extract-gold *tagger* test)))

(deftest greedy-ap-tagger-quality ()
  (should be = 96.31641
          (accuracy *tagger* *gold*)))
