;;; (c) 2013 Vsevolod Dyomkin

(in-package #:nlp.test-util)
(named-readtables:in-readtable rutils-readtable)


(defun test-file (filename)
  (merge-pathnames (strcat "test/" filename) +project-root+))


(defun test-on-corpus (func corpus expected &key (test 'equal))
  (let ((tp 0)
        (fp 0))
  ))

#+nil
(test-on-corpus #`(tokenize (make 'regex-word-tokenizer) %)
                +brown-corpus+
                (test-file "brown-tokenization.txt"))