;;; (c) 2013 Vsevolod Dyomkin

(in-package #:nlp.corpora)
(named-readtables:in-readtable rutils-readtable)


(defmethod read-corpus ((type (eql :nps-chat)) path)
  (let ((rez (make-corpus :name "NPS Chat Corpus"
                          :groups #{:by-class #{} :by-user #{equal}}))
        raw tokens)
    (fad:walk-directory
     path
     #`(when (string= "xml" (pathname-type %))
         (mv-bind (_ cleans tokens classes users) (read-corpus-file :nps-chat %)
           (declare (ignore _))
           (loop :for clean :in cleans
                 :for class :in classes
                 :for user :in users
                 :for toks :in tokens
                 :for i :from 0 :do
              (let ((text (make-text :name (fmt "~A-~A" (pathname-name %) i)
                                     :clean clean :tokens toks))
                    (cls (mkeyw class)))
                (with-slots (texts groups) rez
                  (push text texts)
                  (unless (get# cls (get# :by-class groups))
                    (set# cls (get# :by-class groups) ()))
                  (push text (get# cls (get# :by-class groups)))
                  (unless (get# user (get# :by-user groups))
                    (set# user (get# :by-user groups) ()))
                  (push text (get# usr (get# :by-user groups)))))))))
    rez))

(defmethod read-corpus-file ((type (eql :nps-chat)) source)
  "Read individual file from the NPS Chat Corpus."
  (cxml:parse source (make 'nps-chat-sax)))

(defmethod map-corpus ((type (eql :nps-chat)) path fn)
  (fad:walk-directory
   path
   #`(when (string= "xml" (pathname-type %))
       (mv-bind (_ cleans tokens) (read-corpus-file :nps-chat %)
         (declare (ignore _))
         (loop :for clean :in cleans
               :for toks :in tokens
               :for i :from 0 :do
            (funcall fn (make-text :name (fmt "~A-~A" (pathname-name %) i)
                                   :clean clean :tokens toks)))))))


;; SAX parsing of NPS XML data

(defclass nps-chat-sax (sax:sax-parser-mixin)
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
      (:post (push (attr "class" attributes) classes)
             (push (attr "user" attributes) users))
      (:t (push (make-token :word (attr "word" attributes)
                            :tag (mkeyw (attr "pos" attributes)))
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