;;; (c) 2014 Vsevolod Dyomkin

(in-package #:nlp.contrib.corpora)
(named-readtables:in-readtable rutils-readtable)


(defstruct (semcor-token (:include token))
  cmd
  lemma
  ot
  wsn
  lexsn
  sep)

(defmethod read-corpus-file ((type (eql :semcor)) in)
  "Read individual file from the Semcor Corpus."
  (cxml:parse in (make 'xml-corpus-sax
                       :token-init 'make-semcor-token
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

(defmethod read-corpus ((type (eql :semcor)) path &key ext)
  (declare (ignore ext))
  (let ((rez (make-corpus :desc "Semcor 3.0")))
    (map-corpus :semcor path #`(push % (corpus-texts rez)))
    rez))

(defmethod map-corpus ((type (eql :semcor)) path fn &key ext)
  (declare (ignore ext))
  (with-open-file (tarball path :element-type '(unsigned-byte 8))
    (archive:do-archive-entries
        (entry (archive:open-archive 'archive:tar-archive
                                     (chipz:make-decompressing-stream
                                      'chipz:gzip tarball)))
      (let ((name (slice (archive:name entry)
                         (1+ (position #\/ (archive:name entry) :from-end t)))))
        (when (starts-with "br-" name)
          (let ((buf (make-array (slot-value entry 'archive::size)
                                 :element-type '(unsigned-byte 8)
                                 :adjustable t :fill-pointer t)))
            (read-sequence buf (archive:entry-stream entry))
            (mv-bind (_ clean tokens paragraphs)
                (read-corpus-file :semcor
                                  (re:regex-replace-all
                                   "&"
                                   (re:regex-replace-all
                                    "=\"?([^ >\"]*)\"?"
                                    (flex:octets-to-string buf :external-format :utf-8)
                                    "=\"\\1\"")
                                   "&amp;"))
              (declare (ignore _))
              (funcall fn (make-text :name name :clean clean :tokens tokens
                                     :paragraphs paragraphs)))))))))
