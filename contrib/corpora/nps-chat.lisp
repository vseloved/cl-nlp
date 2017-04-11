;;; (c) 2013-2017 Vsevolod Dyomkin

(in-package #:nlp.contrib.corpora)
(named-readtables:in-readtable rutilsx-readtable)


(defmethod read-corpus-file ((type (eql :nps-chat)) file &key)
  (with-open-file (in file)
    (read-corpus-file :nps-chat in :file file)))

(defmethod read-corpus-file ((type (eql :nps-chat)) (in stream) &key file)
  "Read individual file from the NPS Chat Corpus."
  (mv-bind (_ cleans all-pars classes users) (cxml:parse in (make 'nps-chat-sax))
    (declare (ignore _))
    (let ((filename (pathname-name file)))
      (values (loop :for clean :in cleans
                    :for par-sent-toks :in all-pars
                    :for i :from 0
                    :collect (make-text :name (fmt "~A-~A" filename i)
                                        :clean clean
                                        :par-sent-toks par-sent-toks))
              (mapcar 'mkeyw classes)
              users))))

(defmethod read-corpus ((type (eql :nps-chat)) path &key (ext "xml"))
  (let ((rez (make-corpus :desc "NPS Chat Corpus"
                          :groups #h(:by-class #h() :by-user #h(equal)))))
    (dofiles (file path :ext ext)
      (with ((texts classes users (read-corpus-file :nps-chat file)))
        (loop :for text :in texts
              :for class :in classes
              :for user :in users :do
              (push text @rez.texts)
              (push text (? @rez.groups :by-class cls))
              (push text (? @rez.groups :by-user user)))))
    rez))

(defmethod map-corpus ((type (eql :nps-chat)) path fn &key (ext "xml"))
  (dofiles (file path :ext ext)
    (dolist (text (read-corpus-file :nps-chat file))
      (call fn text))))


;;; SAX parsing of NPS XML data

(defclass nps-chat-sax (sax-progress-mixin)
  ((texts :initform nil)
   (toks :initform nil)
   (classes :initform nil)
   (users :initform nil)
   (cur-tag :initform nil)
   (cur-toks :initform nil)))

(defmethod sax:start-document ((sax nps-chat-sax))
  ;; do nothing
  )

(defmethod sax:start-element ((sax nps-chat-sax)
                              namespace-uri local-name qname attributes)
  (with-slots (classes users cur-toks cur-tag) sax
    (case (setf cur-tag (mkeyw local-name))
      (:post (push (xml-attr "class" attributes) classes)
             (push (xml-attr "user" attributes) users))
      (:t (push (make-tok :word (xml-attr "word" attributes)
                          :tag (mkeyw (xml-attr "pos" attributes)))
                cur-toks)))))

(defmethod sax:characters ((sax nps-chat-sax) data)
  (when (eql :terminals @sax.cur-tag)
    (push data @sax.texts)))

(defmethod sax:end-element ((sax nps-chat-sax) namespace-uri local-name qname)
  (when (eql :terminals (mkeyw local-name))
    (push (reverse @sax.cur-tokens) @sax.toks)
    (void @sax.cur-toks)))

(defmethod sax:end-document ((sax nps-chat-sax))
  (values nil
          (reverse @sax.texts)
          (reverse @sax.toks)
          (reverse @sax.classes)
          (reverse @sax.users)))
