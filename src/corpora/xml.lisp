;;; (c) 2014 Vsevolod Dyomkin

(in-package #:nlp.corpora)
(named-readtables:in-readtable rutils-readtable)


(defclass xml-corpus-sax (sax:sax-parser-mixin)
  ((token-init :initform 'make-token :initarg :token-init)
   (sentence-class :initform nil :initarg :sentence-class :type sentence)
   (struct-map :initform #h() :initarg :struct-map)
   (attr-map :initform #h() :initarg :attr-map)
   (xml-tags :initform nil)
   (cur-sent :initform nil)
   (cur-par :initform nil)
   (paragraphs :initform nil)
   (sentences :initform nil)
   (raw-text :initform nil))
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
                     (mappend #`(list (lt %) (attr (rt %) attributes))
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
         (:= raw-text (strcat raw-text #\Space data))
         (:= cur-sent (let ((toks (ncore:tokenize ncore:<word-tokenizer> data)))
                        (if sentence-class
                            (make sentence-class :tokens toks)
                            toks)))))
      ((get# :paragraph struct-map)
       (when tag-matches?
         (:= raw-text (strcat raw-text #\Newline data))
         (:= cur-par (mapcar #`(ncore:tokenize ncore:<word-tokenizer> %)
                             (ncore:tokenize ncore:<sentence-splitter> data))))))))

(defmethod sax:start-document ((sax xml-corpus-sax))
  ;; do nothing
  )

(defmethod sax:end-document ((sax xml-corpus-sax))
  (with-slots (struct-map sentences paragraphs raw-text) sax
    (when (in# :token struct-map)
      (reversef sentences))
    (when (in# :sentence struct-map)
      (reversef paragraphs))
    (values nil
            (or raw-text
                (paragraphs->text paragraphs))
            sentences
            paragraphs)))

) ; end of symbol-macrolet


;;; util

(defun paragraphs->text (paragraphs)
  "Get a text string corresponding to a list of lists of tokens PARAGRAPHS."
  (strjoin #\Newline
           (mapcar (lambda (par)
                     (strjoin #\Space
                              (mapcar (lambda (sent)
                                        (strjoin #\Space
                                                 (mapcar #'token-word
                                                         (if (listp sent)
                                                             sent
                                                             (sent-tokens sent)))))
                                      par)))
                   paragraphs)))
