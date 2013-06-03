;;; (c) 2013 Vsevolod Dyomkin

(in-package #:nlp.corpora)
(named-readtables:in-readtable rutils-readtable)


(defparameter +brown-corpus+
  (read-corpus :brown (merge-pathnames "brown/*" *corpora-root*))
  "Brown University Standard Corpus of Present-Day American English.")

(defparameter +nps-chat-corpus+
  (read-corpus :nps-chat (merge-pathnames "nps_chat/*" *corpora-root*))
  "NPS Chat Corpus, Release 1.0 (July 2008).")

#+manually
(defparameter +reuters-corpus+
  (read-corpus :reauters (merge-pathnames "reuters/*" *corpora-root*))
  "Reuters Corpus, Volume 1, English language, 1996-08-20 to 1997-08-19
   (Release date 2000-11-03, Format version 1, correction level 0).")