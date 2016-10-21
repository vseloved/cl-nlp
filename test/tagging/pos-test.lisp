;;; (c) 2015-2016 Vsevolod Dyomkin

(in-package #:ntag)
(named-readtables:in-readtable rutilsx-readtable)


(defun extract-sents (text)
  (mapcar ^(make 'ncore:sent :tokens (ncorp::remove-dummy-tokens %))
          (flatten (ncorp:text-tokenized text) 1)))


(defvar *sents* ())
(ncorp:map-corpus :treebank (corpus-file "onf-wsj/")
                  ^(appendf *sents* (extract-sents %)))

(defparameter *gold* (extract-gold <pos-tagger> (sub *sents* 0 10)))

(deftest greedy-ap-dict-tagger-quality ()
  (should be = 97.95918
          (accuracy <pos-tagger> *gold*)))

(deftest greedy-ap-training ()
  (let ((tagger (make 'greedy-ap-dict-postagger)))
    (train tagger (sub *sents* 0 5))
    (should be >= 95 (accuracy tagger *gold*))))
