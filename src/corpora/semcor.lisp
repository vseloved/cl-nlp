;;; (c) 2014 Vsevolod Dyomkin

(in-package #:nlp.corpora)
(named-readtables:in-readtable rutils-readtable)


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

(defmethod read-corpus ((type (eql :semcor)) path &key ext)
  (declare (ignore ext))
  (let ((rez (make-corpus :desc "Semcor 3.0")))
    (map-corpus :semcor path #`(push % (corpus-texts rez)))
    rez))

(defmethod read-corpus-file ((type (eql :semcor)) in)
  "Read individual file from the Semcor Corpus."
  (cxml:parse in (make 'semcor-sax)))


;; SAX parsing of NPS XML data

(defstruct (semcor-token (:include token))
  cmd
  lemma
  ot
  wsn
  lexsn
  sep)

(defclass semcor-sax (sax:sax-parser-mixin)
  ((cur-tag :initform nil)
   (cur-sent :initform nil)
   (cur-par :initform nil)
   (paragraphs :initform nil)))

(defmethod sax:start-document ((sax semcor-sax))
  ;; do nothing
  )

(defmethod sax:start-element ((sax semcor-sax)
                              namespace-uri local-name qname attributes)
  (with-slots (cur-tag cur-sent) sax
    (:= cur-tag (mkeyw local-name))
    (when (member cur-tag '(:wf :punc))
      (push (make-semcor-token :tag (attr "pos" attributes)
                               :cmd (attr "cmd" attributes)
                               :lemma (attr "lemma" attributes)
                               :wsn (attr "wsn" attributes)
                               :lexsn (attr "lexsn" attributes)
                               :ot (attr "ot" attributes)
                               :sep (attr "sep" attributes))
            cur-sent))))

(defmethod sax:characters ((sax semcor-sax) data)
  (with-slots (cur-tag cur-sent) sax
    (when (member cur-tag '(:wf :punc))
      (:= (token-word (first cur-sent)) data))))

(defmethod sax:end-element ((sax semcor-sax) namespace-uri local-name qname)
  (with-slots (cur-tag cur-sent cur-par tokens paragraphs) sax
    (void cur-tag)
    (case (mkeyw local-name)
      (:s (reversef cur-sent)
          (push cur-sent cur-par)
          (void cur-sent))
      (:p (push cur-par paragraphs)
          (void cur-par)))))

(defmethod sax:end-document ((sax semcor-sax))
  (with-slots (paragraphs) sax
    (reversef paragraphs)
    (values nil
            (strjoin #\Newline
                     (mapcar (lambda (par)
                               (strjoin #\Space
                                        (mapcar (lambda (sent)
                                                  (strjoin #\Space
                                                           (mapcar #'token-word sent)))
                                                par)))
                            paragraphs))
            (reduce #'append paragraphs)
            paragraphs)))
