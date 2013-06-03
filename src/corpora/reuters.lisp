;;; (c) 2013 Vsevolod Dyomkin

(in-package #:nlp.corpora)
(named-readtables:in-readtable rutils-readtable)


(defstruct (reuters-text (:include text))
  "A single text from the Reuters corpus."
  headline
  byline
  dateline)


(defmethod read-corpus ((type (eql :reuters)) path)
  "Expects PATH to be a zip archive of the corpus
   with embedded archives for each day."
  (let ((rez (make-corpus :name "Reuters Corpus"
                          :groups #{:by-date #{equal}})))
    (zip:with-zipfile (zip path)
      (zip:do-zipfile-entries (name entry zip)
        (unless (char= #\/ (last-char name))
          (with-zipped-zip (in entry :raw t)
            (mv-bind (_ text __ ___ date headline byline dateline)
                (read-corpus-file :reuters in)
              (declare (ignore _ __ ___))
              (let ((text (make-reuters-text
                           :name name
                           :clean text
                           :tokens (tokenize ncore:<word-tokenizer> text)
                           :headline headline
                           :byline byline
                           :dateline dateline)))
                (with-slots (texts groups) rez
                  (push text texts)
                  (if (get# date (get# :by-date groups))
                      (set# date (get# :by-date groups) (list text))
                      (push text (get# date (get# :by-date groups)))))))))))))


(defmethod read-corpus-file ((type (eql :reuters)) in)
  "Read individual file from the Reuters Corpus."
  (cxml:parse in (make 'reuters-sax)))

(defmethod map-corpus ((type (eql :reuters)) path fn)
  (zip:with-zipfile (zip path)
    (zip:do-zipfile-entries (name entry zip)
      (unless (char= #\/ (last-char name))
        (with-zipped-zip (in entry :raw t)
          (mv-bind (_ text __ ___ date headline byline dateline)
              (read-corpus-file :reuters in)
            (declare (ignore _ __ ___))
            (funcall fn (make-reuters-text
                         :name name
                         :clean text
                         :tokens (tokenize ncore:<word-tokenizer> text)
                         :headline headline
                         :byline byline
                         :dateline dateline))))))))


;; SAX parsing of Reuters XML data

(defclass reuters-sax (sax:sax-parser-mixin)
  ((id :initform nil)
   (date :initform nil)
   (paragraphs :initform nil)
   (headline :initform nil)
   (byline :initform nil)
   (dateline :initform nil)
   (cur-tag :initform nil)))

(defmethod sax:start-element ((sax reuters-sax)
                              namespace-uri local-name qname attributes)
  (with-slots (id date cur-tag) sax
    (when (eql :newsitem (setf cur-tag (mkeyw local-name)))
      (setf id (parse-integer (attr "itemid" attributes))
            date (attr "date" attributes)))))

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
    (values nil
            (strjoin "~%" (reverse paragraphs))
            nil
            id
            date
            (when headline
              (string-trim +white-chars+ (fmt "~{~A~}" headline)))
            (when byline
              (string-trim +white-chars+ (fmt "~{~A~}" byline)))
            (when dateline
              (string-trim +white-chars+ (fmt "~{~A~}" dateline))))))