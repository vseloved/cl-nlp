;;; (c) 2015 Vsevolod Dyomkin

(in-package #:nlp.contrib.corpora)
(named-readtables:in-readtable rutils-readtable)

;; (deftest read-nps-chat ()
;;   (should be string=
;;           "The Ways and Means Committee will hold a hearing on the bill next Tuesday ."
;;           (read-corpus-file
