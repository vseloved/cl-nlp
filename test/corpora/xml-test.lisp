;;; (c) 2015 Vsevolod Dyomkin

(in-package #:nlp.corpora)
(named-readtables:in-readtable rutils-readtable)

(defun make-token-blah (&rest args &key word tag &allow-other-keys)
  (declare (ignore args))
  (make-token :word word :tag tag))

(deftest parse-xml ()
  (should be string=
          (let ((raw (read-file (corpus-file "samples/semcor-br-c14"))))
            (slice #1=(2nd (cxml:parse (re:regex-replace-all
                                        "&" (re:regex-replace-all
                                             "=\"?([^ >\"]*)\"?"
                                             raw
                                             "=\"\\1\"")
                                        "&amp;")
                                       (make 'xml-corpus-sax
                                             :token-init 'make-token-blah
                                             :struct-map #h(:token '(:wf :punc)
                                                            :sentence :s
                                                            :paragraph :p)
                                             :attr-map #h(:tag "pos"
                                                          :cmd "cmd"
                                                          :lemma "lemma"
                                                          :wsn "wsn"
                                                          :lexsn "lexsn"
                                                          :ot "ot"
                                                          :sep "sep"))))
                   0 (1+ (position #\. #1#))))
          "Elisabeth_Schwarzkopf sang so magnificently Saturday night at Hunter_College that it seems a pity to have_to register any complaints ."))
