;;; (c) 2014-2017 Vsevolod Dyomkin

(in-package #:nlp.contrib.corpora)
(named-readtables:in-readtable rutilsx-readtable)


(defstruct (semcor-tok (:include tok))
  cmd
  ot
  wsn
  lexsn
  sep)

(defmethod read-corpus-file ((type (eql :semcor)) in &key)
  "Read individual file from the Semcor Corpus."
  (cxml:parse in (make 'xml-corpus-sax
                       :token-init 'make-semcor-token
                       :struct-map #h(:tok '(:wf :punc)
                                      :sent :s
                                      :par :p)
                       :attr-map #h(:tag "pos"
                                    :cmd "cmd"
                                    :lemma "lemma"
                                    :wsn "wsn"
                                    :lexsn "lexsn"
                                    :ot "ot"
                                    :sep "sep"))))

(defmethod read-corpus ((type (eql :semcor)) path &key ext)
  (let ((rez (make-corpus :desc "Semcor 3.0")))
    (map-corpus :semcor path ^(push % @rez.texts) :ext ext)
    rez))

(deftype ub8 () '(unsigned-byte 8))

(defmethod map-corpus ((type (eql :semcor)) path fn &key ext)
  (declare (ignore ext))
  (with-open-file (tarball path :element-type 'ub8)
    (archive:do-archive-entries
        (entry (archive:open-archive 'archive:tar-archive
                                     (chipz:make-decompressing-stream
                                      'chipz:gzip tarball)))
      (let ((name (slice (archive:name entry)
                         (1+ (position #\/ (archive:name entry) :from-end t)))))
        (when (starts-with "br-" name)
          (let ((buf (make-array (slot-value entry 'archive::size)
                                 :element-type 'ub8
                                 :adjustable t :fill-pointer t)))
            (read-sequence buf (archive:entry-stream entry))
            (with ((_ clean parag-sent-toks
                      (read-corpus-file :semcor
                                        (-> buf
                                            (flex:octets-to-string
                                             % :external-format :utf-8)
                                            (re:regex-replace-all
                                             "=\"?([^ >\"]*)\"?" % "=\"\\1\"")
                                            (re:regex-replace-all "&" % "&amp;")))))
              (call fn (make-text :name name
                                  :clean clean
                                  :parag-sent-toks parag-sent-toks)))))))))
