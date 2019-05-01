;;; (c) 2016-2017 Vsevolod Dyomkin

(in-package #:nlp.core)
(named-readtables:in-readtable rutilsx-readtable)


(def-lang-profile :uk
  :word-tags (dict-from-file (lang-file :uk "word-tags.txt")
                             :test 'eql :key-transform 'tag:export-tag)
  :word-tokenizer
  (make 'regex-word-tokenizer
        :regex (regex-from-file (lang-file :uk "word-tok-rules.txt")))
  :sent-splitter
  (make 'punct-sent-tokenizer
        :abbrevs-with-dot (list-from-file
                           (lang-file :uk "abbrevs-with-dot.txt")))
  :dict-lemmatizer
  (gzip-stream:with-open-gzip-file (in (lang-file :uk "dict.txt.gz"))
    (nlex:load-mem-dict (flex:make-flexi-stream in :external-format :utf-8))))
                            
