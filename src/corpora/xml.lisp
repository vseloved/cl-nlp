;;; (c) 2014-2017 Vsevolod Dyomkin

(in-package #:nlp.corpora)
(named-readtables:in-readtable rutilsx-readtable)


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
  ((tok-init :initform 'make-tok :initarg :tok-init)
   (sent-class :initform nil :initarg :sent-class :type sent)
   (struct-map :initform #h() :initarg :struct-map)
   (attr-map :initform #h() :initarg :attr-map)
   (xml-tags :initform nil  :accessor sax-xml-tags)
   (cur-sent :initform nil)
   (cur-parag :initform nil)
   (parags :initform nil :accessor sax-parags)
   (sents :initform nil :accessor sax-sents)
   (raw-text :initform nil :accessor sax-raw-text))
  (:documentation
   "Generic XML corpus sax parser distinguishes up to 3 levels:
    paragraph / sentence / token (STRUCT-MAP slot provides the name of tags
    that correspond to each level, or a list of names).
    SENTS collect tokens, PARAGS are a list of sentences,
    and RAW-TEXT contatins the extracted text as a continuous string
    (it may be either 'restored' from the XML representation,
    or aggregated directly from the more general elements (sentence, paragraphs)
    if they are not split into tokens).
    Each read token may have a specific TOK-INIT function
    and a number of attributes whose names in the token class
    and in the XML are identified by ATTR-MAP.
    If SENT-CLASS is setup, tokens will be groupped not into lists
    but into objects of this class (which should b e a subclass of SENT)."))


(symbol-macrolet ((tag-matches? (member (first xml-tags) (mklist it))))

(defmethod sax:start-element ((sax xml-corpus-sax)
                              namespace-uri local-name qname attributes)
  (declare (ignore namespace-uri qname))
  (with-slots (tok-init struct-map attr-map xml-tags cur-sent cur-parag) sax
    (push (mkeyw local-name) xml-tags)
    (when-it (get# :tok struct-map)
      (when tag-matches?
        (push (apply tok-init
                     (flat-map ^(list (lt %) (xml-attr (rt %) attributes))
                               (ht->pairs attr-map)))
              cur-sent)))))

(defmethod sax:end-element ((sax xml-corpus-sax) namespace-uri local-name qname)
  (declare (ignore namespace-uri local-name qname))
  (with-slots (struct-map xml-tags cur-sent cur-parag sents parags sent-class)
      sax
    (cond
      ((and-it (get# :sent struct-map) tag-matches?)
       (when (and (in# :tok struct-map)
                  (not sent-class))
         (reversef cur-sent))
       (when (in# :parag struct-map)
         (push cur-sent cur-parag))
       (push cur-sent sents)
       (void cur-sent))
      ((and-it (get# :parag struct-map) tag-matches?)
       (when (in# :sent struct-map)
         (reversef cur-parag))
       (push cur-parag parags)
       (void cur-parag)))
    (pop xml-tags)))

(defmethod sax:characters ((sax xml-corpus-sax) data)
  (with-slots (struct-map sent-class cur-tag cur-sent cur-parag raw-text xml-tags)
      sax
    (cond-it
      ;; if we have token level we ignore other levels
      ((get# :tok struct-map)
       (when tag-matches?
         (:= (tok-word (first cur-sent)) data)))
      ;; if we have sentence level we ignore paragraph level
      ((get# :sent struct-map)
       (when tag-matches?
         (:= raw-text (strcat raw-text #\Space data)
             cur-sent (let ((toks (tokenize <word-tokenizer> data)))
                        (if sent-class
                            (make sent-class :toks toks)
                            toks)))))
      ((get# :parag struct-map)
       (when tag-matches?
         (:= raw-text (strcat raw-text #\Newline data)
             cur-parag (mapcar ^(tokenize <word-tokenizer> %)
                               (tokenize <sent-splitter> data))))))))

(defmethod sax:start-document ((sax xml-corpus-sax))
  ;; do nothing
  )

(defmethod sax:end-document ((sax xml-corpus-sax))
  (with-slots (struct-map sents parags raw-text) sax
    (if (in# :sent struct-map)
        (reversef parags)
        (:= parags (list sents)))
    (values nil
            (or raw-text
                (parags->text parags))
            parags)))

) ; end of symbol-macrolet
