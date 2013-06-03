;;; (c) 2013 Vsevolod Dyomkin

(in-package #:nlp.contrib.wordnet)
(named-readtables:in-readtable rutils-readtable)

;;; Link types

(defvar *link-types* nil
  "linktype - linkid mapping.")

(defun %init-link-types ()
  "Load linkid - linktype mapping from WORDNET."
  (setf *link-types* (make-hash-table))
  (ensure-db <wordnet>)
  (loop :for (linkid link-type)
        :in (query (select '(linkid link :from linktypes))) :do
     (set# (mkeyw (substitute #\- #\Space link-type)) *link-types* linkid))
  *link-types*)

(defun link-types ()
  (or *link-types*
      (%init-link-types)))


(defmethod lemma ((wordnet sql-wordnet3) (wordid integer))
  (query1 (select 'lemma `(:where wordid := ,wordid))))

(defmethod lemmas ((wordnet sql-wordnet3) (synset synset))
  (query (select 'lemma
                 `(:where wordid
                   :in ,(select '(wordid :from senses)
                                `(:where synsetid := ,(synset-id synset)))))))

(defmethod words ((wordnet sql-wordnet3) (synset synset))
  (mapcar #'lemma-word (lemmas wordnet synset)))

(defmethod synset ((wordnet sql-wordnet3) (lemma lemma))
  (query1 (select 'synset
                  `(:where synsetid
                    := ,(select '(synsetid :from senses)
                                `(:where wordid := ,(lemma-id lemma)))))))

(defmethod synset ((wordnet sql-wordnet3) (key string))
  (ds-bind (word pos sensenum) (split #\. key)
    (query1 (select 'synset
                    `(:where pos := ,(esc pos)
                      :and synsetid
                      :in ,(select '(synsetid :from senses)
                                   `(:where sensenum := ,(parse-integer sensenum)
                                     :and wordid
                                     := ,(select '(wordid :from words)
                                                 `(:where lemma
                                                   := ,(esc word))))))))))

(defmethod synsets ((wordnet sql-wordnet3) (word string) &key pos)
  (query (select 'synset
                 `(:where synsetid
                   :in ,(select '(synsetid :from senses)
                                `(:where wordid
                                  := ,(select '(wordid :from words)
                                              `(:where lemma := ,(esc word)))
                                  ,@(when pos `(:and pos := ,(esc pos)))))))))

(defmethod sense ((wordnet sql-wordnet3) (key string))
  (ds-bind (word synset-key) (split #\- key)
    (let ((synset (synset wordnet synset-key)))
      (query1 (select 'sense
                      `(:where wordid
                        :in ,(select '(wordid :from words)
                                     `(:where lemma := ,(esc word)))
                        :and synsetid := ,(synset-id synset)))))))

(defmethod senses ((wordnet sql-wordnet3) (word string) &key pos)
  (query (select 'sense
                 `(:where wordid
                   := ,(select '(wordid :from words)
                               `(:where lemma := ,(esc word)))
                   ,@(when pos `(:and pos := ,(esc pos)))))))

(defmethod samples ((wordnet sql-wordnet3) (synset synset))
  (query (select 'sample
                 `(:where synsetid := ,(synset-id synset)))))

(defmethod examples ((wordnet sql-wordnet3) (word string))
  (mapcar #'sample-example (mapcan #'samples (synsets word))))

(defmethod examples ((wordnet sql-wordnet3) (synset synset))
  (mapcar #'sample-example (samples wordnet synset)))

(defmethod related ((wordnet sql-wordnet3) (synset synset) (link-type symbol)
                    &key reverse)
  (query (select 'synset
                 `(:where synsetid
                   :in ,(if reverse
                           (select '(synset1id :from semlinks)
                                   `(:where synset2id := ,(synset-id synset)
                                     :and linkid := ,(get# link-type
                                                           (link-types))))
                           (select '(synset2id :from semlinks)
                                   `(:where synset1id := ,(synset-id synset)
                                     :and linkid := ,(get# link-type
                                                           (link-types)))))))))

(defmethod related ((wordnet sql-wordnet3) (sense sense) (link-type symbol)
                    &key reverse)
  (when-it
      (query1 (select 'lexlink
                      (if reverse
                          `(:where word2id := ,(sense-word-id sense)
                            :and synset2id := ,(sense-synset-id sense)
                            :and linkid := ,(get# link-type (link-types)))
                          `(:where word1id := ,(sense-word-id sense)
                            :and synset1id := ,(sense-synset-id sense)
                            :and linkid := ,(get# link-type (link-types))))))
    (query (select 'sense
                   (if reverse
                       `(:where wordid := ,(link-word1-id it)
                         :and synsetid := ,(link-synset1-id it))
                       `(:where wordid := ,(link-word2-id it)
                         :and synsetid := ,(link-synset2-id it)))))))

(defmethod lowest-common-hypernyms ((wordnet sql-wordnet3)
                                    (synset1 synset) (synset2 synset))
  (if (eql synset1 synset2)
      (values (list synset1)
              0
              0)
      (do* ((depth 1 (1+ depth))
            (cur (list synset1 synset2) next)
            (next () ())
            (reached (list (cons synset1 0) (cons synset2 0)))
            rez)
           ((null cur))
        (dolist (s cur)
          (dolist (h (related wordnet s :hypernym))
            (if-it (find h reached :key 'car)
                   (push it rez)
                   (progn
                     (push h next)
                     (push (cons h depth) reached)))))
        (when rez
          (return (values (mapcar #'car rez)
                          depth
                          (cdr (car rez))))))))


(defmethod min-depth ((wordnet sql-wordnet3) (synset synset))
  (do* ((depth 0 (1+ depth))
        (cur (list synset) next)
        (next () ()))
      (nil)
    (dolist (s cur)
      (if-it (related wordnet s :hypernym)
             (dolist (h it)
               (push h next))
             (return-from min-depth depth)))))

(defmethod hypernym-paths ((wordnet sql-wordnet3) (synset synset))
  (do* ((cur (list (cons synset ())) next)
        (next () ())
        paths)
      ((null cur) paths)
    (dolist (s cur)
      (let ((p (cons (car s) (cdr s))))
        (if-it (related wordnet (car s) :hypernym)
               (dolist (h it)
                 (push (cons h p) next))
               (push p paths))))))

(defmethod path-similarity ((wordnet sql-wordnet3)
                            (synset1 synset) (synset2 synset))
  (mv-bind (_ l1 l2) (lowest-common-hypernyms wordnet synset1 synset2)
    (declare (ignore _))
    (when l1
      (/ 1 (+ l1 l2 1)))))

(defmethod lch-similarity ((wordnet sql-wordnet3)
                           (synset1 synset) (synset2 synset))
  (mv-bind (lch l1 l2) (lowest-common-hypernyms wordnet synset1 synset2)
    (when l1
      (- (log (/ (+ l1 l2 1)
                 (* 2 (max-depth wordnet synset1))))))))

(defmethod max-depth ((wordnet sql-wordnet3) (synset synset))
  (reduce #'max (mapcar #`(or (get# % *taxonomy-depths*)
                              (progn
                                (format *debug-io*
                                        "Calculating taxonomy depth for ~A~%" %)
                                (set# % *taxonomy-depths*
                                      (calc-max-depth wordnet %))))
                        (mapcar #'car (hypernym-paths wordnet synset)))))

(defmethod calc-max-depth ((wordnet sql-wordnet3) (synset synset))
  (do* ((depth 0 (1+ depth))
        (cur (list synset) next)
        (next () ()))
      (nil)
    (dolist (s cur)
      (dolist (h (related wordnet s :hyponym))
        (push h next)))
    (unless next
      (return depth))))

(defmethod wup-similarity ((wordnet sql-wordnet3)
                           (synset1 synset) (synset2 synset))
  (mv-bind (lch l1 l2) (lowest-common-hypernyms wordnet synset1 synset2)
    (reduce #'max
            (mapcar #`(let ((2d (* 2 (min-depth wordnet %))))
                        (/ 2d (+ l1 l2 2d)))
                    lch))))

(defmethod res-similarity ((wordnet sql-wordnet3)
                           (synset1 synset) (synset2 synset))
  (reduce #'max
          (mapcar #'information-content
                  (lowest-common-hypernyms wordnet synset1 synset2))))

(defmethod jcn-similarity ((wordnet sql-wordnet3)
                           (synset1 synset) (synset2 synset))
  (/ 1
     (+ (information-content synset1) (information-content synset2)
        (- (* 2 (reduce #'min
                        (mapcar #'information-content
                                (lowest-common-hypernyms wordnet
                                                         synset1 synset2))))))))

(defmethod lin-similarity ((wordnet sql-wordnet3)
                           (synset1 synset) (synset2 synset))
  (/ (* 2 (reduce #'min
                  (mapcar #'information-content
                          (lowest-common-hypernyms wordnet synset1 synset2))))
     (+ (information-content synset1) (information-content synset2))))
