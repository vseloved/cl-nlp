;;; (c) 2014-2017 Vsevolod Dyomkin

(in-package #:nlp.parsing)
(named-readtables:in-readtable rutilsx-readtable)

(declaim (inline read-stanford-dep read-conll-dep print-stanford-dep))


(defstruct (dep (:print-object print-stanford-dep))
  (rel nil :type (or symbol null))
  (head nil :type (or tok null))
  (child nil :type (or tok null)))

(defgeneric print-dep (format dep &optional stream)
  (:documentation
   "FORMAT may be :stanford, :conll, and, maybe some other.")
  (:method ((format (eql :stanford)) dep &optional (stream *standard-output*))
      (format stream "~(~A~)(~A-~A, ~A-~A)"
              @dep.rel @dep.head.word @dep.head.id @dep.child.word @dep.child.id))
  (:method ((format (eql :conll)) dep &optional (stream *standard-output*))
    (format stream "~A	~A	~:[_~;~:*~A~]	_	~A	~A~%"
            @dep.child.id @dep.child.word @dep.child.lemma @dep.child.pos
            @dep.head.id @dep.rel)))

(defun print-stanford-dep (dep stream)
  "Print DEP in Stanford dependency format to STREAM."
  (print-dep :stanford dep stream))

(defgeneric read-dep (format str &optional toks)
  (:documentation
   "Read one dependency in FORMAT from STR.
    TOKS is a cache of already encountered tokens.")
  (:method ((format (eql :stanford)) str &optional (toks #h(-1 dep:+root+)))
    (with ((split1 (position #\( str))
           (split2 (position #\Space str
                             :start (position #\Space str :start (1+ split1)
                                                          :test-not 'eql)))
           (split3 (position #\) str :from-end t))
           ((head head-id) (split #\- (string-trim +white-chars+
                                                   (slice str (1+ split1)
                                                          split2))))
           (head-id (parse-integer head-id :junk-allowed t))
           ((child child-id) (split #\- (string-trim +white-chars+
                                                     (slice str (1+ split2)
                                                            split3))))
           (child-id (parse-integer child-id))
           (rel (mksym (slice str 0 split1) :package :dep)))
      (make-dep
       :rel rel
       :head (if (eql 'dep:root rel)
                 (? toks -1)
                 (getset# head-id toks
                          (make-tok :id head-id :word head)))
       :child (getset# child-id toks
                       (make-tok :id child-id :word child)))))
  (:method ((format (eql :conll)) str &optional (toks #h(-1 dep:+root+)))
    (with (((id word lemma pos pos2 feats head-id rel &rest rest)
            (split #\Tab str :remove-empty-subseqs t)))
      (declare (ignore pos2 feats rest))
      (let ((child-id (parse-integer id))
            (head-id (parse-integer head-id))
            (rel rel :package :dep))
        (make-dep
         :rel rel
         :head (or (? toks (if (eql 'dep:root rel) -1 head-id))
                   (make-tok :id head-id))
         :child (getset# child-id toks
                         (make-tok :id child-id
                                   :word word
                                   :lemma (unless (string= "_" lemma)
                                            lemma)
                                   :pos (mksym pos :package :tag))))))))

(defgeneric read-deps (format str)
  (:documentation
   "Read a dependency parse sturcture in a given FORMAT from STR.
    Returns a list of list of dependencies for each sentence.")
  (:method (format (str string))
    (with-input-from-string (in str)
      (read-deps format in)))
  (:method (format (str stream))
    (let ((toks #h(-1 dep:+root+))
          deps
          all-deps)
      (loop :for line := (read-line str nil) :while line :do
        (if (blankp line)
            (progn
              (:= toks #h(-1 dep:+root+))
              (when deps (push (reverse deps) all-deps))
              (void deps))
            (push (read-dep format line toks) deps))
         :finally (push (reverse deps) all-deps))
      (reverse all-deps))))

(defun deps->tree (deps &optional dep)
  (when deps
    (unless dep (:= dep (find 'dep:+root+ deps :key 'dep-rel)))
    (cons dep
          (sort (cons @dep.child
                      (mapcar ^(deps->tree deps %)
                              (keep-if ^(eql @dep.child %) deps
                                       :key 'dep-head)))
                '< :key ^(tok-id (etypecase %
                                   (tok %)
                                   (list @%.child#0)))))))

#+nil
(defun pprint-deps-tree (deps)
  (let ((*package* (find-package :dep)))
    (pprint-tree (maptree #`(etypecase %
                              (dep (dep-rel %))
                              (tok (tok-word %)))
                          (deps->tree deps)))))
