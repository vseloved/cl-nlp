;;; (c) 2014 Vsevolod Dyomkin

(in-package #:ntag)
(named-readtables:in-readtable rutils-readtable)


(defun extract-sents (text)
  (mapcar #`(make 'ncore:sentence :tokens (ncorp:remove-dummy-tokens %))
          (ncorp:text-tokens text)))

(defvar *tagger* (load-model (make 'greedy-ap-dict-tagger)
                             (model-file "pos-tagging/wsj.zip")))

(defvar *sents* ())
(ncorp:map-corpus :treebank (corpus-file "onf-wsj/")
                  #`(appendf *sents* (extract-sents %)))

(defvar *gold* (extract-gold *tagger* (sub *sents* 0 5)))

(deftest greedy-ap-dict-tagger-quality ()
  (should be = 97.64706
          (accuracy *tagger* *gold*)))

(deftest greedy-ap-training ()
  (let ((tagger (make 'greedy-ap-dict-tagger)))
    (train tagger (sub *sents* 0 5))
    (should be >= 95 (accuracy tagger *gold*))))
