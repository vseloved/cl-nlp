;;; (c) 2013-2014 Vsevolod Dyomkin

(in-package #:nlp.tags)
(named-readtables:in-readtable rutils-readtable)


(defvar *tags* (dict-from-file (src-file "tags/tags.txt")
                               :key-transform #`(export (intern %)))
  "Tags.")

(defvar *phrase-tags* (dict-from-file (src-file "tags/phrase-tags.txt")
                                      :key-transform #`(export (intern %)))
  "Phrase tags.")
