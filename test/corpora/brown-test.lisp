;;; (c) 2015 Vsevolod Dyomkin

(in-package #:nlp.corpora)
(named-readtables:in-readtable rutils-readtable)

(deftest read-brown ()
  (should be string=
          "The Fulton County Grand Jury said Friday an investigation of Atlanta's recent primary election produced \" no evidence \" that any irregularities took place ."
          (slice #1=(text-clean (read-corpus-file :brown (corpus-file "brown/ca01")))
                 0 (1+ (position #\. #1#)))))
