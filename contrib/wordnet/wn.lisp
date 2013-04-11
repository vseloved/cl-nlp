;;; (c) 2013 Vsevolod Dyomkin

(in-package #:nlp.contrib.wn)
(named-readtables:in-readtable rutils-readtable)


(declaim (inline lemma lemmas words synset synsets senses samples examples
                 related lch min-depth root-hypernyms))

(defun lemma (wordid)
  "LEMMA for WORDID with <WORDNET>."
  (wordnet:lemma wordnet:<wordnet> wordid))

(defun lemmas (synset)
  "LEMMAS for SYNSET with <WORDNET>."
  (wordnet:lemmas wordnet:<wordnet> synset))

(defun words (synset)
  "WORDS for SYNSET with <WORDNET>."
  (wordnet:words wordnet:<wordnet> synset))

(defun synset (key)
  "SYNSET for KEY with <WORDNET>."
  (wordnet:synset wordnet:<wordnet> key))

(defun synsets (word &key pos)
  "SYNSETS for WORD with optional POS with <WORDNET>."
  (wordnet:synsets wordnet:<wordnet> word :pos pos))

(defun sense (key)
  "SENSE for KEY with <WORDNET>."
  (wordnet:sense wordnet:<wordnet> key))

(defun senses (word &key pos)
  "SENSES for WORD with optional POS with <WORDNET>."
  (wordnet:senses wordnet:<wordnet> word :pos pos))

(defun samples (synset)
  "SAMPLES for SYNSET with <WORDNET>."
  (wordnet:samples wordnet:<wordnet> synset))

(defun examples (obj)
  "EXAMPLES for OBJ with <WORDNET>."
  (wordnet:examples wordnet:<wordnet> obj))

(defun related (obj link-type &key reverse)
  (wordnet:related wordnet:<wordnet> obj link-type :reverse reverse))

(defun lch (synset1 synset2)
  (wordnet:lowest-common-hypernyms wordnet:<wordnet> synset1 synset2))

(defun min-depth (synset)
  "MIN-DEPTH for SYNSET with <WORDNET>."
  (wordnet:min-depth wordnet:<wordnet> synset))

(defun hypernym-paths (synset)
  "HYPERNYM-PATHS for SYNSET with <WORDNET>."
  (wordnet:hypernym-paths wordnet:<wordnet> synset))

(defun path-similarity (synset1 synset2)
  "PATH-SIMILARITY for SYNSET1 and SYNSET2 with <WORDNET>."
  (wordnet:path-similarity wordnet:<wordnet> synset1 synset2))

(defun lch-similarity (synset1 synset2)
  "LCH-SIMILARITY for SYNSET1 and SYNSET2 with <WORDNET>."
  (wordnet:lch-similarity wordnet:<wordnet> synset1 synset2))

(defun wup-similarity (synset1 synset2)
  "WUP-SIMILARITY for SYNSET1 and SYNSET2 with <WORDNET>."
  (wordnet:wup-similarity wordnet:<wordnet> synset1 synset2))

(defun res-similarity (synset1 synset2)
  "RES-SIMILARITY for SYNSET1 and SYNSET2 with <WORDNET>."
  (wordnet:res-similarity wordnet:<wordnet> synset1 synset2))

(defun jcn-similarity (synset1 synset2)
  "JCN-SIMILARITY for SYNSET1 and SYNSET2 with <WORDNET>."
  (wordnet:jcn-similarity wordnet:<wordnet> synset1 synset2))

(defun lin-similarity (synset1 synset2)
  "LIN-SIMILARITY for SYNSET1 and SYNSET2 with <WORDNET>."
  (wordnet:lin-similarity wordnet:<wordnet> synset1 synset2))

