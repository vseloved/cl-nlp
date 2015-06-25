;;; (c) 2014-2015 Vsevolod Dyomkin

(in-package #:nlp.corpora)
(named-readtables:in-readtable rutils-readtable)


(defun xml-attr (name attributes)
  "Shortut XML attribute accessor."
  (when-it (find name attributes :test 'string=
                 :key #'sax::standard-attribute-local-name)
    (sax::standard-attribute-value it)))

(defclass sax-progress-mixin (sax:sax-parser-mixin)
  ((progress-count :initform 0 :accessor sax-progress-count)
   (progress-report-rate :initform 1000 :initarg :report-rate
                         :accessor sax-progress-report-rate)
   (tracking-tag :initarg :tracking-tag :accessor sax-progress-tracking-tag)))

(defmethod sax:start-document :before ((sax sax-progress-mixin))
  (:= (sax-progress-count sax) 0))

(defmethod end-element :after ((sax sax-progress-mixin)
                               namespace-uri local-name qname)
  (when (and-it (sax-progress-tracking-tag sax)
                (eql (mkeyw local-name) it)
                (zerop (/ (:+ (sax-progress-count sax))
                          (sax-progress-report-rate sax))))
    (princ ".")))

(defclass xml-corpus-sax (sax-progress-mixin)
  ((token-init :initform 'make-token :initarg :token-init)
   (sentence-class :initform nil :initarg :sentence-class :type sentence)
   (struct-map :initform #h() :initarg :struct-map)
   (attr-map :initform #h() :initarg :attr-map)
   (xml-tags :initform nil  :accessor sax-xml-tags)
   (cur-sent :initform nil)
   (cur-par :initform nil)
   (paragraphs :initform nil :accessor sax-paragraphs)
   (sentences :initform nil :accessor sax-sentences)
   (raw-text :initform nil :accessor sax-raw-text))
  (:documentation
   "Generic XML corpus sax parser distinguishes up to 3 levels:
    paragraph / sentence / token (STRUCT-MAP slot provides the name of tags
    that correspond to each level, or a list of names).
    SENTENCES collect tokens, PARAGRAPHS are a list of sentences,
    and RAW-TEXT contatins the extracted text as a continuous string
   (it may be either 'restored' from the XML representation,
    or aggregated directly from the more general elements (sentence, paragraphs)
    if they are not split into tokens).
    Each read token may have a specific TOKEN-INIT function
    and a number of attributes whose names in the token class
    and in the XML are identified by ATTR-MAP.
    If SENTENCE-CLASS is setup, tokens will be groupped not into lists
    but into objects of this class (which should b e a subclass of SENTENCE)."))


(symbol-macrolet ((tag-matches? (member (first xml-tags) (mklist it))))

(defmethod sax:start-element ((sax xml-corpus-sax)
                              namespace-uri local-name qname attributes)
  (declare (ignore namespace-uri qname))
  (with-slots (token-init struct-map attr-map xml-tags cur-sent cur-par) sax
    (push (mkeyw local-name) xml-tags)
    (when-it (get# :token struct-map)
      (when tag-matches?
        (push (apply token-init
                     (flat-map #`(list (lt %) (xml-attr (rt %) attributes))
                               (ht->pairs attr-map)))
              cur-sent)))))

(defmethod sax:end-element ((sax xml-corpus-sax) namespace-uri local-name qname)
  (declare (ignore namespace-uri local-name qname))
  (with-slots (struct-map xml-tags cur-sent cur-par sentences paragraphs sentence-class) sax
    (cond
      ((and-it (get# :sentence struct-map) tag-matches?)
       (when (and (in# :token struct-map)
                  (not sentence-class))
         (reversef cur-sent))
       (when (in# :paragraph struct-map)
         (push cur-sent cur-par))
       (push cur-sent sentences)
       (void cur-sent))
      ((and-it (get# :paragraph struct-map) tag-matches?)
       (when (in# :sentence struct-map)
         (reversef cur-par))
       (push cur-par paragraphs)
       (void cur-par)))
    (pop xml-tags)))

(defmethod sax:characters ((sax xml-corpus-sax) data)
  (with-slots (struct-map sentence-class cur-tag cur-sent cur-par raw-text xml-tags) sax
    (cond-it
      ;; if we have token level we ignore other levels
      ((get# :token struct-map)
       (when tag-matches?
         (:= (token-word (first cur-sent)) data)))
      ;; if we have sentence level we ignore paragraph level
      ((get# :sentence struct-map)
       (when tag-matches?
         (:= raw-text (strcat raw-text #\Space data)
             cur-sent (let ((toks (ncore:tokenize ncore:<word-tokenizer> data)))
                        (if sentence-class
                            (make sentence-class :tokens toks)
                            toks)))))
      ((get# :paragraph struct-map)
       (when tag-matches?
         (:= raw-text (strcat raw-text #\Newline data)
             cur-par (mapcar #`(ncore:tokenize ncore:<word-tokenizer> %)
                             (ncore:tokenize ncore:<sentence-splitter> data))))))))

(defmethod sax:start-document ((sax xml-corpus-sax))
  ;; do nothing
  )

(defmethod sax:end-document ((sax xml-corpus-sax))
  (with-slots (struct-map sentences paragraphs raw-text) sax
    (if (in# :sentence struct-map)
        (reversef paragraphs)
        (:= paragraphs (list sentences)))
    (values nil
            (or raw-text
                (paragraphs->text paragraphs))
            paragraphs)))

) ; end of symbol-macrolet
