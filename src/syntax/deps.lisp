;;; (c) 2013-2016 Vsevolod Dyomkin

(in-package #:nlp.deps)
(named-readtables:in-readtable rutilsx-readtable)


(def-lang-var dep-tags
    (dict-from-file (lang-file :en "dep-tags.txt")
                    :test 'eql
                    :key-transform ^(tag:export-tag % (find-package '#:nlp.deps)))
  "Dependency labels."
  :greedy t)

(defvar +root+ (ncore:make-token :id 0 :word "_ROOT_"))

(tag:export-tag "ROOT" (find-package '#:nlp.deps))
