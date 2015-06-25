;;; (c) 2015 Vsevolod Dyomkin

(in-package #:nlp.contrib.corpora)
(named-readtables:in-readtable rutils-readtable)


(defmethod read-corpus-file ((type (eql :wikipedia)) path &key preserve-markup?)
  (mv-bind (_ texts tokenizeds titles)
      (cxml:parse-file path (make 'wiki-sax :preserve-markup? preserve-markup?))
    (declare (ignore _))
    (mapcar (lambda (title clean tokenized)
              (make-text :name title
                         :clean clean
                         :tokenized tokenized))
            titles texts tokenizeds)))

(defmethod read-corpus ((type (eql :wikipedia)) path &key ext preserve-markup?)
  (declare (ignore ext))
  (make-corpus
   :desc "Wikipedia"
   :texts (read-corpus-file :wikipedia path :preserve-markup? preserve-markup?)))

(defmethod map-corpus ((type (eql :wikipedia)) path fn &key ext preserve-markup?)
  (declare (ignore ext))
  (cxml:parse-file path (make 'wiki-stream-sax :fn fn
                              :preserve-markup? preserve-markup?)))


;; SAX parsing of Wikipedia data

(defclass wiki-sax (sax:sax-parser-mixin)
  ((preserve-markup? :initform nil :initarg :preserve-markup?)
   (texts :initform nil)
   (tokens :initform nil)
   (titles :initform nil)
   (cur-tag :initform nil)
   (cur-text :initform nil)
   (cur-title :initform nil)
   (skip :initform nil)))

(defclass wiki-stream-sax (wiki-sax)
  ((fn :initarg :fn)))


(defmethod sax:start-document ((sax wiki-sax))
  ;; do nothing
  )

(defmethod sax:start-element ((sax wiki-sax)
                              namespace-uri local-name qname attributes)
  (with-slots (cur-tag skip) sax
    (:= skip nil
        cur-tag (mkeyw local-name))))

(defmethod sax:characters ((sax wiki-sax) data)
  (with-slots (cur-tag cur-title cur-text skip preserve-markup?) sax
    (case cur-tag
      (:title (push data cur-title))
      (:text (if (starts-with "#redirect" data :test 'string-equal)
                 (:= skip t)
                 (let ((raw (-> data
                                (re:regex-replace-all "&amp;nbsp;" % " ")
                                (re:regex-replace-all "&quot;" % "'")
                                (re:regex-replace-all "&amp;" % "&"))))
                   (push (if preserve-markup?
                             raw
                             (-> raw
                                 (re:regex-replace-all "[\\[\\]]" % "")
                                 (re:regex-replace-all "&[gl]t;" % " ")))
                         cur-text)))))))

(defmethod sax:end-element ((sax wiki-sax) namespace-uri local-name qname)
  (with-slots (cur-tag cur-title cur-text texts tokens titles skip) sax
    (when (eql :text cur-tag)
      (unless skip
        (let ((text (strjoin #\Space (reverse cur-text))))
          (push (string-trim +white-chars+ (strjoin #\Space (reverse cur-title)))
                titles)
          (push text texts)
          (push (tokenize <full-text-tokenizer> text) tokens)))
      (void cur-text)
      (void cur-title))))

(defmethod sax:end-element ((sax wiki-stream-sax) namespace-uri local-name qname)
  (with-slots (cur-tag cur-title cur-text fn skip) sax
    (when (eql :text cur-tag)
      (unless skip
        (let ((text (strjoin #\Space (reverse cur-text))))
          (funcall fn (make-text
                       :name (string-trim +white-chars+
                                          (strjoin #\Space (reverse cur-title)))
                       :clean text
                       :tokenized (tokenize <full-text-tokenizer> text)))))
      (void cur-text)
      (void cur-title))))

(defmethod sax:end-document ((sax wiki-sax))
  (with-slots (texts titles tokens) sax
    (values nil
            (reverse texts)
            (reverse tokens)
            (reverse titles))))

(defmethod sax:end-document ((sax wiki-stream-sax))
  ;; do nothing
  )
