;;; (c) 2015-2017 Vsevolod Dyomkin

(in-package #:ntag)
(named-readtables:in-readtable rutilsx-readtable)


(defun extract-sents (text)
  (mapcar ^(make 'ncore:sent :toks (ncorp::remove-dummy-tokens %))
          (flatten @text.parag-sent-toks 1)))


(defvar *sents* ())
(ncorp:map-corpus :treebank (corpus-file "onf-wsj/")
                  ^(appendf *sents* (extract-sents %)))

(defparameter *gold* (extract-gold <pos-tagger> (subseq *sents* 0 10)))

(deftest greedy-ap-dict-tagger-quality ()
  (should be = 97.95918
          (accuracy <pos-tagger> *gold*)))

#+nil ; TODO: find why accuracy dropped
(deftest greedy-ap-training ()
  (let ((tagger (make 'greedy-ap-dict-postagger)))
    (train tagger (subseq *sents* 0 5))
    (should be >= 95 (accuracy tagger *gold*))))
