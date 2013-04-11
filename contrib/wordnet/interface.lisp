;;; (c) 2013 Vsevolod Dyomkin

(in-package #:nlp.contrib.wordnet)
(named-readtables:in-readtable rutils-readtable)


(defgeneric lemma (wordnet wordid)
  (:documentation
   "Get lemma by its WORDID.")
  (:method :before ((wordnet sql-wordnet3) wordid)
    (ensure-db wordnet)))

(defgeneric lemmas (wordnet synset)
  (:documentation
   "Get a list of lemma objects for a SYNSET.")
  (:method :before ((wordnet sql-wordnet3) synset)
    (ensure-db wordnet)))

(defgeneric words (wordnet synset)
  (:documentation
   "Get a list of word strings for SYNSET."))

(defgeneric synset (wordnet key)
  (:documentation
   "Get a specific synset by KEY")
  (:method :before ((wordnet sql-wordnet3) key)
    (ensure-db wordnet)))

(defgeneric synsets (wordnet word &key pos)
  (:documentation
   "Get a list of synset objects for string WORD,
    optionally limited to pos tag POS.")
  (:method :before ((wordnet sql-wordnet3) word &key pos)
    (ensure-db wordnet)))

(defgeneric senses (wordnet word &key pos)
  (:documentation
   "Get a list of sense objects for string WORD,
    optionally limited to pos tag POS.")
  (:method :before ((wordnet sql-wordnet3) word &key pos)
    (ensure-db wordnet)))

(defgeneric samples (wordnet synset)
  (:documentation
   "Get a list of sample objects for SYNSET.")
  (:method :before ((wordnet sql-wordnet3) synset)
    (ensure-db wordnet)))

(defgeneric examples (wordnet obj)
  (:documentation
   "Get a list of example strings for OBJ."))

(defgeneric related (wordnet obj link-type &key reverse)
  (:documentation
   "Find all synsets related to OBJ by LINK-TYPE relationship
   (or vice verse if REVERSE is supplied).")
  (:method :before ((wordnet sql-wordnet3) obj link-type &key reverse)
    (ensure-db wordnet)))