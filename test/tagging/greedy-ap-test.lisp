;;; (c) 2014 Vsevolod Dyomkin

(in-package #:ntag)
(named-readtables:in-readtable rutils-readtable)


(defun extract-sents (text)
  (mapcar #`(make 'ncore:sentence :tokens (ncorp:remove-dummy-tokens %))
          (ncorp:text-tokens text)))

(defpar *tagger2* (load-model (make 'greedy-ap-tagger)
                             (model-file "pos-tagging/onf.zip")
                             :classes-package :tag))
(defpar *gold2*
  (let (test)
    (ncorp:map-corpus :treebank (corpus-file "onf-wsj/")
                      #`(appendf test (extract-sents %)))
    (extract-gold *tagger2* test)))

(deftest greedy-ap-tagger-quality ()
  (should be = 96.31641
          (accuracy *tagger2* *gold2*)))
