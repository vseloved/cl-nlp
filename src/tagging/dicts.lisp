;;; (c) 2014-2016 Vsevolod Dyomkin

(in-package #:nlp.tagging)
(named-readtables:in-readtable rutilsx-readtable)


(defun build-single-pos-words-dict (sents &key ignore-case?
                                    (freq-threshold 20) (ambiguity-threshold 0.97))
  (let ((posdict (make-hash-table :test (if ignore-case? 'equalp 'equal))))
    (dolist (sent sents)
      (dolist (tok sent)
        (with-accessors ((pos token-pos) (word token-word)) tok
          (let ((poscounts (or (get# word posdict)
                               (set# word posdict #h()))))
            (:+ (get# pos poscounts 0))))))
    (dotable (word pos-weights posdict)
      (mv-bind (argmax max) (argmax 'rt (ht->pairs pos-weights))
        (let ((total (reduce '+ (ht-vals pos-weights))))
          (if (and (> total freq-threshold)
                   (> (/ max total) ambiguity-threshold))
              (set# word posdict (lt argmax))
              (rem# word posdict)))))
    posdict))
