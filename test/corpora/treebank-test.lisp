;;; (c) 2013-2015 Vsevolod Dyomkin

(in-package #:nlp.corpora)
(named-readtables:in-readtable rutils-readtable)

(deftest read-treebank ()
  (should be equal '((S (|``| "\"")
                        (NLP.TAGS::NP-SBJ "I")
                        (VP "do" "not"
                            (VP "mind"
                                (S (NLP.TAGS::NP-SBJ "you(r)")
                                   (VP "leaving"
                                       (NLP.TAGS::ADV-TMP "early")))))))
          (with-tmp-file (f "(S (`` \")
                                (NP-SBJ I)
                                (VP do not
                                    (VP mind
                                        (S (NP-SBJ you(r))
                                           (VP leaving
                                               (ADV-TMP early))))))")
            (text-trees (read-corpus-file :treebank f))))
  (should be string=
          "The Ways and Means Committee will hold a hearing on the bill next Tuesday ."
          (last1 (split #\Newline
                        (text-clean (read-corpus-file :treebank
                                                      (corpus-file "onf-wsj/wsj_2200.parse")))
                        :remove-empty-subseqs t))))
