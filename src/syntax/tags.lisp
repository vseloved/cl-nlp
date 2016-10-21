;;; (c) 2013-2016 Vsevolod Dyomkin

(in-package #:nlp.tags)
(named-readtables:in-readtable rutils-readtable)


(defun export-tag (str &optional (package (find-package :nlp.tags)))
  "Intern and export STR in package, then return it."
  (let ((tag (intern str)))
    (export tag package)
    tag))

(def-lang-var word-tags (dict-from-file (lang-file :en "word-tags.txt")
                                        :test 'eql :key-transform 'export-tag)
  "Word-level tags."
  :greedy t)

(def-lang-var phrase-tags (dict-from-file (lang-file :en "phrase-tags.txt")
                                          :test 'eql :key-transform 'export-tag)
  "Phrase-level tags."
  :greedy t)

(export-tag "-NONE-")
