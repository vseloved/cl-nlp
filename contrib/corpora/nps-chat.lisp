;;; (c) 2013-2015 Vsevolod Dyomkin

(in-package #:nlp.contrib.corpora)
(named-readtables:in-readtable rutils-readtable)


(defmethod read-corpus-file ((type (eql :nps-chat)) file &key)
  (with-open-file (in file)
    (read-corpus-file :nps-chat in :file file)))

(defmethod read-corpus-file ((type (eql :nps-chat)) (in stream) &key file)
  "Read individual file from the NPS Chat Corpus."
  (mv-bind (_ cleans tokenizeds classes users) (cxml:parse in (make 'nps-chat-sax))
    (declare (ignore _))
    (let ((filename (pathname-name file)))
      (values (loop :for clean :in cleans
                    :for tokenized :in tokenizeds
                    :for i :from 0
                    :collect (make-text :name (fmt "~A-~A" filename i)
                                        :clean clean :tokenized tokenized))
              (mapcar 'mkeyw classes)
              users))))

(defmethod read-corpus ((type (eql :nps-chat)) path &key (ext "xml"))
  (let ((rez (make-corpus :desc "NPS Chat Corpus"
                          :groups #h(:by-class #h() :by-user #h(equal)))))
    (with-slots (texts groups) rez
      (dofiles (file path :ext ext)
        (mv-bind (texts classes users) (read-corpus-file :nps-chat file)
          (loop :for text :in texts
                :for class :in classes
                :for user :in users :do
                (push text texts)
                (push text (get# cls (get# :by-class groups)))
                (push text (get# user (get# :by-user groups)))))))
    rez))

(defmethod map-corpus ((type (eql :nps-chat)) path fn &key (ext "xml"))
  (dofiles (file path :ext ext)
    (dolist (text (read-corpus-file :nps-chat file))
      (funcall fn text))))


;;; SAX parsing of NPS XML data

(defclass nps-chat-sax (sax-progress-mixin)
  ((texts :initform nil)
   (tokens :initform nil)
   (classes :initform nil)
   (users :initform nil)
   (cur-tag :initform nil)
   (cur-tokens :initform nil)))

(defmethod sax:start-document ((sax nps-chat-sax))
  ;; do nothing
  )

(defmethod sax:start-element ((sax nps-chat-sax)
                              namespace-uri local-name qname attributes)
  (with-slots (classes users cur-tokens cur-tag) sax
    (case (setf cur-tag (mkeyw local-name))
      (:post (push (xml-attr "class" attributes) classes)
             (push (xml-attr "user" attributes) users))
      (:t (push (make-token :word (xml-attr "word" attributes)
                            :tag (mkeyw (xml-attr "pos" attributes)))
                cur-tokens)))))

(defmethod sax:characters ((sax nps-chat-sax) data)
  (with-slots (cur-tag texts) sax
    (when (eql :terminals cur-tag)
      (push data texts))))

(defmethod sax:end-element ((sax nps-chat-sax) namespace-uri local-name qname)
  (when (eql :terminals (mkeyw local-name))
    (with-slots (tokens cur-tokens) sax
      (push (reverse cur-tokens) tokens)
      (setf cur-tokens nil))))

(defmethod sax:end-document ((sax nps-chat-sax))
  (with-slots (texts tokens users classes) sax
    (values nil
            (reverse texts)
            (reverse tokens)
            (reverse classes)
            (reverse users))))
