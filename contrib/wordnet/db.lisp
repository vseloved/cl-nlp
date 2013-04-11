;;; (c) 2013 Vsevolod Dyomkin

(in-package #:nlp.contrib.wordnet)
(named-readtables:in-readtable rutils-readtable)


;;; Connection handling

(defmacro with-wordnet ((wordnet) &body body)
  "A macro to wrap all Wordnet DB interaction."
  `(%call-with-wordnet wordnet (lambda () ,@body)))

(defun %call-with-wordnet (wordnet fn)
  "Helper function for WITH-WORDNET macro"
  (clsql:with-database (clsql:*default-database* (list (wordnet-uri wordnet))
                                                 :database-type :sqlite3)
    (funcall fn)))

(defun connect-wordnet (wordnet)
  "Connect to Wordnet DB."
  (clsql:connect (list (wordnet-uri wordnet)) :database-type :sqlite3))

(defun ensure-db (wordnet)
  "Unless we are connected to WORDNET db,
   connect to it as CLSQL:*DEFAULT-DATABASE*."
  (unless clsql:*default-database*
    (setf clsql:*default-database* (connect-wordnet wordnet))))


;;; Entities

(clsql:def-view-class lemma ()
  ((wordid :initarg :wordid :reader lemma-id
           :type integer :db-kind :key)
   (lemma :initarg :lemma :reader lemma-word
          :type string :db-constraints (:not-null :unique)))
  (:base-table "words"))

(clsql:def-view-class synset ()
  ((synsetid :initarg :synsetid :reader synset-id
             :type integer :db-kind :key)
   (pos :initarg :pos :reader synset-pos
        :type char)
   (lexdomainid :initarg :lexdomainid
                :type integer :db-constraints (:not-null))
   (definition :initarg :definition :reader synset-def
               :type string)
   (word :initarg :word :reader synset-word :db-kind :virtual)
   (sensenum :initarg :sensenum :db-kind :virtual))
  (:base-table "synsets"))

(defmethod synset-pos ((synset synset))
  (char (slot-value synset 'pos) 0))

(clsql:def-view-class sense ()
  ((senseid :initarg :senseid :reader sense-id
            :type integer :db-constraints (:unique))
   (wordid :initarg :wordid :reader sense-word-id
           :type integer :db-kind :key)
   (casedwordid :initarg :casedwordid :reader sense-casedword-id
                :type integer)
   (synsetid :initarg :synsetid :reader sense-synset-id
             :type integer :db-kind :key)
   (sensenum :initarg :sensenum
             :type integer :db-constraints (:not-null))
   (lexid :initarg :lexid
          :type integer :db-constraints (:not-null))
   (tagcount :initarg :tagcount
             :type integer)
   (sensekey :initarg :sensekey
             :type string :db-constraints (:unique))
   (word :initarg :word :db-kind :virtual)
   (synset :initarg :synset :db-kind :virtual))
  (:base-table "senses"))

(clsql:def-view-class sample ()
  ((synsetid :initarg :synsetid
             :type integer :db-kind :key)
   (sampleid :initarg :sampleid :reader sample-id
             :type integer :db-kind :key)
   (sample :initarg :sample :reader sample-example
           :type string :db-constraints (:not-null)))
  (:base-table "samples"))

(clsql:def-view-class link-type ()
  ((linkid :initarg :linkid :reader link-id
           :type integer :db-kind :key)
   (link :initarg :link :reader link-type
         :type string)
   (recurses :initarg :recurses
             :type integer :db-constraints (:not-null)))
  (:base-table "linktypes"))

(clsql:def-view-class semlink ()
  ((synset1id :initarg :synset1id :reader link-synset1-id
              :type integer :db-kind :key)
   (synset2id :initarg :synset2id :reader link-synset2-id
              :type integer :db-kind :key)
   (linkid :initarg :linkid :reader link-id
           :type integer :db-kind :key))
  (:base-table "semlinks"))

(clsql:def-view-class lexlink ()
  ((word1id :initarg :word1id :reader link-word1-id
            :type integer :db-kind :key)
   (synset1id :initarg :synset1id :reader link-synset1-id
              :type integer :db-kind :key)
   (word2id :initarg :word2id :reader link-word2-id
            :type integer :db-kind :key)
   (synset2id :initarg :synset2id :reader link-synset2-id
              :type integer :db-kind :key)
   (linkid :initarg :linkid :reader link-id
           :type integer :db-kind :key))
  (:base-table "lexlinks"))


;;; Virtual slot initilaizers

(defmethod slot-unbound (class (synset synset) (slot (eql 'word)))
  (ensure-db <wordnet>)
  (setf (slot-value synset slot)
        (query1 (select '(lemma :from words)
                        `(:where wordid
                          := ,(select '(wordid :from senses)
                                      `(:where synsetid := ,(synset-id synset)
                                        :limit 1))
                          :limit 1)))))

(defmethod slot-unbound (class (synset synset) (slot (eql 'sensenum)))
  (ensure-db <wordnet>)
  (setf (slot-value synset slot)
        (query1 (select '(sensenum :from senses)
                        `(:where synsetid := ,(synset-id synset)
                          :limit 1)))))

(defmethod slot-unbound (class (sense sense) (slot (eql 'word)))
  (ensure-db <wordnet>)
  (setf (slot-value sense slot)
        (query1 (select '(lemma :from words)
                        `(:where wordid
                          := ,(sense-word-id sense)
                          :limit 1)))))

(defmethod slot-unbound (class (sense sense) (slot (eql 'synset)))
  (ensure-db <wordnet>)
  (setf (slot-value sense slot)
        (query1 (select 'synset
                        `(:where synsetid := ,(sense-synset-id sense)
                          :limit 1)))))

(defmethod synset-name ((synset synset))
  (with-slots (word pos sensenum) synset
    (fmt "~A.~A.~A" word pos sensenum)))

(defmethod sense-name ((sense sense))
  (with-slots (word synset sensenum) sense
    (fmt "~A~~~A.~A.~A" word (synset-word synset) (synset-pos synset) sensenum)))


;;; Print functions

(defmethod print-object ((lemma lemma) stream)
  (print-unreadable-object (lemma stream :type t :identity t)
    (with-slots (lemma wordid) lemma
      (format stream "~A ~A" lemma wordid))))

(defmethod print-object ((synset synset) stream)
  (print-unreadable-object (synset stream :type t :identity t)
    (format stream "~A ~A" (synset-name synset) (synset-id synset))))

(defmethod print-object ((sense sense) stream)
  (print-unreadable-object (sense stream :type t :identity t)
    (format stream "~A ~A" (sense-name sense) (sense-id sense))))

(defmethod print-object ((sample sample) stream)
  (print-unreadable-object (sample stream :type t :identity t)
    (with-slots (sample sampleid) sample
      (format stream "~A ~A" sample sampleid))))

(defmethod print-object ((semlink semlink) stream)
  (print-unreadable-object (semlink stream :type t :identity t)
    (with-slots (word1id synset1id word2id synset2id linkid) semlink
      (format stream "~A -~A-> ~A"
              synset1id
              (car (find linkid (ht->alist (link-types))
                         :key 'cdr))
              synset2id))))

(defmethod print-object ((lexlink lexlink) stream)
  (flet ((get-word (word-id)
           (or (ignore-errors (lemma-word (lemma <wordnet> word-id)))
               word-id)))
    (print-unreadable-object (lexlink stream :type t :identity t)
      (with-slots (word1id synset1id word2id synset2id linkid) lexlink
        (format stream "~A:~A -~A-> ~A:~A"
                (get-word word1id) synset1id
                (car (find linkid (ht->alist (link-types)) :key 'cdr))
                (get-word word2id) synset2id)))))