;;; (c) 2015-206 Vsevolod Dyomkin

(in-package #:nlp.corpora)
(named-readtables:in-readtable rutilsx-readtable)

(defun make-tok-blah (&rest args &key word pos &allow-other-keys)
  (declare (ignore args))
  (make-tok :word word :pos pos))

(deftest parse-xml ()
  (should be string=
          (let ((raw (read-file (corpus-file "samples/semcor-br-c14"))))
            (slice #1=(2nd (cxml:parse (-> raw
                                           (re:regex-replace-all
                                            "=\"?([^ >\"]*)\"?" % "=\"\\1\"")
                                           (re:regex-replace-all
                                            "&" % "&amp;"))
                                       (make 'xml-corpus-sax
                                             :tok-init 'make-tok-blah
                                             :struct-map #h(:tok '(:wf :punc)
                                                            :sent :s
                                                            :parag :p)
                                             :attr-map #h(:tag "pos"
                                                          :cmd "cmd"
                                                          :lemma "lemma"
                                                          :wsn "wsn"
                                                          :lexsn "lexsn"
                                                          :ot "ot"
                                                          :sep "sep"))))
                   0 (1+ (position #\. #1#))))
          "Elisabeth_Schwarzkopf sang so magnificently Saturday night at Hunter_College that it seems a pity to have_to register any complaints ."))
