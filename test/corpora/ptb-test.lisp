;;; (c) 2015 Vsevolod Dyomkin

(in-package #:nlp.contrib.corpora)
(named-readtables:in-readtable rutils-readtable)

(deftest read-ptb ()
  (should be string= "Pierre Vinken , 61 years old , will join the board as a nonexecutive director Nov."
          (slice #1=(text-clean (read-corpus-file :ptb-tagged
                                                  (corpus-file "samples/WSJ_0001.POS")))
                 0 (1+ (position #\. #1#)))))
