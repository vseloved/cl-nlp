;;; (c) 2013-2017 Vsevolod Dyomkin

(in-package #:nlp.corpora)
(named-readtables:in-readtable rutilsx-readtable)

(deftest read-treebank ()
  (should be equal '((tag:S (tag:|``| "\"")
                      (tag::NP-SBJ "I")
                      (tag:VP "do" "not"
                       (tag:VP "mind"
                        (tag:S (tag::NP-SBJ "you(r)")
                         (tag:VP "leaving"
                          (tag::ADV-TMP "early")))))))
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
