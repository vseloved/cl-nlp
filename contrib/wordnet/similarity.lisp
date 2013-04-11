;;; (c) 2013 Vsevolod Dyomkin

(in-package #:nlp.contrib.wordnet)
(named-readtables:in-readtable rutils-readtable)


(defgeneric lowest-common-hypernyms (wordnet synset1 synset2)
  (:documentation
   "Get a list of lowest common hypernyms synset of SYNSET1 and SYNSET2.
    Return length of both paths to the common hypernyms as 2nd and 3rd values.")
  (:method :before ((wordnet sql-wordnet3) synset1 synset2)
    (ensure-db wordnet)))

(defgeneric min-depth (wordnet synset)
  (:documentation
   "Shortest path length from current SYNSET to root via hypernym relation.")
  (:method :before ((wordnet sql-wordnet3) synset)
    (ensure-db wordnet)))

(defgeneric hypernym-pathss (wordnet synset)
  (:documentation
   "Get the paths to root hypernyms of a SYNSET.")
  (:method :before ((wordnet sql-wordnet3) synset)
    (ensure-db wordnet)))

(defgeneric path-similarity (wordnet synset1 synset2)
  (:documentation
   "Return a score denoting how similar are SYNSET1 and SYNSET2,
    based on the shortest path that connects them in the is-a taxonomy.
    The score is in the range 0 to 1, 1 represents identity,
    nil indicates that path can't be found.")
  (:method :before ((wordnet sql-wordnet3) synset1 synset2)
    (ensure-db wordnet)))

(defgeneric lch-similarity (wordnet synset1 synset2)
  (:documentation
   "Leacock-Chodorow Similarity:
    Return a score denoting how similar are SYNSET1 and SYNSET2,
    based on the shortest path that connects them in the is-a taxonomy
    and maximum depth of the taxonomy, in which they occur.
    The score is caluclaterd as (- (log (/ p (* 2 d))))
    where p is the shortest path length and d the taxonomy depth.
    Nil indicates that path can't be found.")
  (:method :before ((wordnet sql-wordnet3) synset1 synset2)
    (ensure-db wordnet)))

(defgeneric wup-similarity (wordnet synset1 synset2)
  (:documentation
   "Wu-Palmer Similarity:
    Return a score denoting how similar are SYNSET1 and SYNSET2,
    based on the depth of them in the taxonomy
    and the depth of their Least Common Subsumer.
    The relationship is given by the equation:

        (/ (* 2 (depth lcs)) (+ (depth s1) (depth s2)))

    Nil indicates that they don't have a common subsumer.")
  (:method :before ((wordnet sql-wordnet3) synset1 synset2)
    (ensure-db wordnet)))

(defgeneric res-similarity (wordnet synset1 synset2)
  (:documentation
   "Resnik Similarity:
    Return a score denoting how similar are SYNSET1 and SYNSET2,
    based on the Information Content (IC) of the Least Common Subsumer.
    Nil indicates that they don't have a common subsumer.")
  (:method :before ((wordnet sql-wordnet3) synset1 synset2)
    (ensure-db wordnet)))

(defgeneric jcn-similarity (wordnet synset1 synset2)
  (:documentation
   "Jiang-Conrath Similarity:
    Return a score denoting how similar are SYNSET1 and SYNSET2,
    based on the Information Content (IC) of them and of their
    Least Common Subsumer. The relationship is given by the equation:

        (/ 1 (+ (IC s1) (IC s2) (- (* 2 (IC lcs)))))

    Nil indicates that they don't have a common subsumer.")
  (:method :before ((wordnet sql-wordnet3) synset1 synset2)
    (ensure-db wordnet)))

(defgeneric lin-similarity (wordnet synset1 synset2)
  (:documentation
   "Lin Similarity:
    Return a score denoting how similar are SYNSET1 and SYNSET2,
    based on the Information Content (IC) of them and of their
    Least Common Subsumer. The relationship is given by the equation:

        (/ (* 2 (IC lcs)) (+ (IC s1) (IC s2)))

    Nil indicates that they don't have a common subsumer.")
  (:method :before ((wordnet sql-wordnet3) synset1 synset2)
    (ensure-db wordnet)))


;;; Taxonomy depth

(defvar *taxonomy-depths* (make-hash-table)
  "Depths of Wordnet taxonomy for various all the nodes.")

(defgeneric max-depth (wordnet synset)
  (:documentation
   "Return the maximum depth of the WORDNET taxonomy below SYNSET.")
  (:method :before ((wordnet sql-wordnet3) synset)
    (ensure-db wordnet)))

(defgeneric calc-max-depth (wordnet synset)
  (:documentation
   "Calculate the maximum depth of the WORDNET taxonomy below SYNSET.")
  (:method :before ((wordnet sql-wordnet3) synset)
    (ensure-db wordnet)))