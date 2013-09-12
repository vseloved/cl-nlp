;;; (c) 2013 Vsevolod Dyomkin

(in-package #:nlp.corpora)
(named-readtables:in-readtable rutils-readtable)


(defun make-corpus-from-dir (name dir &key (test 'identity))
  "Make a corpus named NAME of texts from files in DIR.
   Optionally, filenames may need to satisfy TEST."
  (let (texts)
    (fad:walk-directory dir
                        #`(when (funcall test (pathname-name %))
                            (push (pair (pathname-name %)
                                        (read-file %))
                                  texts)))
    (setf texts (mapcar #`(make-text
                           :name (l %)
                           :raw (r %)
                           :tokens
                           (mapcar #`(ncore:make-token :word %)
                                   (mapcan #`(ncore:tokenize
                                              ncore:<word-tokenizer> %)
                                           (ncore:tokenize
                                            ncore:<sentence-splitter> (r %)))))
                        texts))
    (make-corpus :desc name
                 :texts texts)))


;;; pre-defined corpora

(defparameter +brown-corpus+
  (read-corpus :brown (merge-pathnames "brown/*" *corpora-root*))
  "Brown University Standard Corpus of Present-Day American English.")

(defparameter +nps-chat-corpus+
  (read-corpus :nps-chat (merge-pathnames "nps_chat/*" *corpora-root*))
  "NPS Chat Corpus, Release 1.0 (July 2008).")

#+manually
(defparameter +reuters-corpus+
  (read-corpus :reuters (merge-pathnames "reuters/*" *corpora-root*))
  "Reuters Corpus, Volume 1, English language, 1996-08-20 to 1997-08-19
   (Release date 2000-11-03, Format version 1, correction level 0).")
