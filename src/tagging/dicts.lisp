;;; (c) 2014 Vsevolod Dyomkin

(in-package #:nlp.tagging)
(named-readtables:in-readtable rutils-readtable)


(defvar *single-tag-words*)
  ;; (dict-from-file (data-file "dicts/single-tag-words.txt")
  ;;                 :val-transform #`(intern % :nlp)))


(defun build-single-tag-words-dict (sents &key ignore-case?
                                    (freq-threshold 20) (ambiguity-threshold 0.97))
  (let ((tagdict (make-hash-table :test (if ignore-case? 'equalp 'equal))))
    (dolist (sent sents)
      (dolist (token sent)
        (with-accessors ((tag token-tag) (word token-word)) token
          (let ((tagcounts (or (get# word tagdict)
                               (set# word tagdict #h()))))
            (incf (get# tag tagcounts 0))))))
    (dotable (word tags-weights tagdict)
      (mv-bind (argmax max) (argmax 'rt (ht->pairs tags-weights))
        (let ((total (reduce '+ (ht-vals tags-weights))))
          (if (and (> total freq-threshold)
                   (> (/ max total) ambiguity-threshold))
              (set# word tagdict (lt argmax))
              (rem# word tagdict)))))
    tagdict))
