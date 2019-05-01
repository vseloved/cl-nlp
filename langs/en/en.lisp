;;; (c) 2019 Vsevolod Dyomkin

(in-package #:nlp.core)
(named-readtables:in-readtable rutilsx-readtable)


(def-lang-profile :en
  :word-tags (dict-from-file (lang-file :en "word-tags.txt")
                             :test 'eql :key-transform 'tag:export-tag)
  :dep-tags (dict-from-file (lang-file :en "dep-tags.txt")
                            :test 'eql :key-transform 'tag:export-tag)
  :phrase-tags (dict-from-file (lang-file :en "phrase-tags.txt")
                               :test 'eql :key-transform 'tag:export-tag)
  :word-tokenizer (make 'regex-word-tokenizer
                        :regex (regex-from-file
                                (lang-file :en "word-tok-rules.txt")))
  :sent-splitter (make 'punct-sent-tokenizer
                       :abbrevs-with-dot (list-from-file
                                          (lang-file :en "abbrevs-with-dot.txt")))
  :dict-lemmatizer (nlex:load-mem-dict (lang-file :en "wikt-dict.txt"))
  :stopwords (dict-from-file (lang-file :en "stopwords.txt")
                             :test 'equalp :val-transform (constantly t)))
