;;; (c) 2016 Vsevolod Dyomkin

(in-package #:nlp.core)
(named-readtables:in-readtable rutilsx-readtable)


(defprofile :uk
  :word-tags (dict-from-file (lang-file :uk "word-tags.txt")
                             :test 'eql :key-transform 'export-tag)
  :dict (load-dict (lang-file :uk "dict.txt.gz"))
  :word-tokenizer
  (make 'regex-word-tokenizer
        :regex (regex-from-file (lang-file :uk "word-tok-rules.txt")))
  :sentence-splitter
  (make 'baseline-sentence-tokenizer
        :abbrevs-with-dot (list-from-file
                           (lang-file :uk "abbrevs-with-dot.txt"))))

