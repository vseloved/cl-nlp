;;; (c) 2013-2015 Vsevolod Dyomkin

(in-package #:nlp.contrib.corpora)
(named-readtables:in-readtable rutils-readtable)


(defstruct (reuters-text (:include text))
  "A single text from the Reuters corpus."
  headline
  byline
  dateline)


(defmethod read-corpus-file ((type (eql :reuters-rcv1)) in &key name)
  "Read individual file from the Reuters Corpus."
  (mv-bind (_ clean tokenized __ date headline byline dateline)
      (cxml:parse in (make 'reuters-sax))
    (declare (ignore _ __))
    (values (make-reuters-text :name name
                               :clean clean
                               :tokenized tokenized
                               :headline headline
                               :byline byline
                               :dateline dateline)
            date)))

(defmacro do-reuters-texts ((path text &optional date) &body body)
  (with-gensyms (zip entry name subname in)
    `(zip:with-zipfile (,zip ,path)
       (zip:do-zipfile-entries (,name ,entry ,zip)
         (unless (char= #\/ (last-char ,name))
           (with-zipped-zip (,subname ,in ,entry :raw t)
             (mv-bind (,text ,@(when date (list date)))
                 (read-corpus-file :reuters-rcv1 ,in
                                   :name (strcat ,name "/" ,subname))
               ,@body)))))))

(defmethod read-corpus ((type (eql :reuters-rcv1)) path &key ext)
  "Expects PATH to be a zip archive of the corpus
   with embedded archives for each day."
  (declare (ignore ext))
  (let ((rez (make-corpus :desc "Reuters Corpus"
                          :groups #h(:by-date #h(equal)))))
    (do-reuters-texts (path text date)
      (with-slots (texts groups) rez
        (push text texts)
        (if (in# date (? groups :by-date))
            (push text (? groups :by-date date))
            (set# date (? groups :by-date) (list text)))))))

(defmethod map-corpus ((type (eql :reuters-rcv1)) path fn &key ext)
  (declare (ignore ext))
  (do-reuters-texts (path text)
    (funcall fn text)))


;; SAX parsing of Reuters XML data

(defclass reuters-sax (sax:sax-parser-mixin)
  ((id :initform nil)
   (date :initform nil)
   (paragraphs :initform nil)
   (headline :initform nil)
   (byline :initform nil)
   (dateline :initform nil)
   (cur-tag :initform nil)))

(defmethod sax:start-document ((sax reuters-sax))
  ;; do nothing
  )

(defmethod sax:start-element ((sax reuters-sax)
                              namespace-uri local-name qname attributes)
  (with-slots (id date cur-tag) sax
    (:= cur-tag (mkeyw local-name))
    (when (eql :newsitem cur-tag)
      (:= id (parse-integer (xml-attr "itemid" attributes))
          date (xml-attr "date" attributes)))))

(defmethod sax:characters ((sax reuters-sax) data)
  (with-slots (cur-tag text paragraphs headline byline dateline) sax
    (case cur-tag
      (:headline (push data headline))
      (:byline (push data byline))
      (:dateline (push data dateline))
      (:p (push data paragraphs)))))

(defmethod sax:end-element ((sax reuters-sax) namespace-uri local-name qname)
  ;; do nothing
  )

(defmethod sax:end-document ((sax reuters-sax))
  (with-slots (id date paragraphs headline byline dateline) sax
    (let ((text (strjoin #\Newline (reverse paragraphs))))
      (values nil
              text
              (tokenize <full-text-tokenizer> text)
              id
              date
              (when headline
                (string-trim +white-chars+ (fmt "~{~A~}" headline)))
              (when byline
                (string-trim +white-chars+ (fmt "~{~A~}" byline)))
              (when dateline
                (string-trim +white-chars+ (fmt "~{~A~}" dateline)))))))
