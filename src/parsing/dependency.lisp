;;; (c) 2014-2016 Vsevolod Dyomkin

(in-package #:nlp.parsing)
(named-readtables:in-readtable rutilsx-readtable)

(declaim (inline read-stanford-dep read-conll-dep print-stanford-dep))


(defstruct (dep (:print-object print-stanford-dep))
  (rel nil :type (or symbol null))
  (govr nil :type (or token null))
  (dept nil :type (or token null)))

(defgeneric print-dep (format dep &optional stream)
  (:documentation
   "FORMAT may be :stanford, :conll, and, maybe some other.")
  (:method ((format (eql :stanford)) dep &optional (stream *standard-output*))
    (with-slots (rel govr dept) dep
      (format stream "~(~A~)(~A-~A, ~A-~A)" rel
              (token-word govr) (token-id govr)
              (token-word dept) (token-id dept))))
  (:method ((format (eql :conll)) dep &optional (stream *standard-output*))
    (with-slots (rel govr dept) dep
      (with-slots (id word lemma pos) dept
        (format stream
                "~A	~A	~:[_~;~:*~A~]	_	~A	~A~%"
                id word lemma pos (token-id govr) rel)))))

(defun print-stanford-dep (dep stream)
  "Print DEP in Stanford dependency format to STREAM."
  (print-dep :stanford dep stream))

(defun read-stanford-dep (str &optional (tokens #h(0 +root+)))
  "Read one Stanford format dependency from STR.
   TOKENS is a cache of already encountered tokens."
  (let* ((split1 (position #\( str))
         (split2 (position #\, str))
         (split3 (position #\) str))
         (govr (split #\- (string-trim +white-chars+
                                       (slice str (1+ split1) split2))))
         (govr-idx (parse-integer (second govr)))
         (dept (split #\- (string-trim +white-chars+
                                       (slice str (1+ split2) (1- split3)))))
         (dept-idx (parse-integer (second dept))))
    (make-dep :rel (mksym (slice str 0 split1) :package :deps)
              :govr (getset# govr-idx tokens
                             (make-token :id govr-idx :word (first govr)))
              :dept (getset# dept-idx tokens
                             (make-token :id dept-idx :word (first dept))))))

(defun read-conll-dep (str &optional (tokens #h(0 +root+)))
  "Read one CONLL format dependency from STR.
   TOKENS is a cache of already encountered tokens."
  (ds-bind (id word lemma pos pos2 feats head-id rel &rest rest)
      (split #\Tab str :remove-empty-subseqs t)
    (declare (ignore pos2 feats rest))
    (let ((dept-id (parse-integer id))
          (govr-id (parse-integer head-id)))
      (make-dep :rel (mksym rel :package :dep)
                :govr (or (? tokens govr-id)
                          (make-token :id govr-id))
                :dept (getset# dept-id tokens
                               (make-token :id dept-id
                                           :word word
                                           :lemma (unless (string= "_" lemma)
                                                    lemma)
                                           :pos (mksym pos :package :tag)))))))

(defgeneric read-deps (format str)
  (:documentation
   "Read a dependency parse sturcture in a given FORMAT from STR.
    Returns a list of list of dependencies for each sentence.")
  (:method (format (str string))
    (with-input-from-string (in str)
      (call-next-method format in)))
  (:method ((format (eql :stanford)) (str stream))
    (let (tokens all-deps deps)
      (loop :for line := (read-line str nil) :while line :do
         (if (blankp line)
             (progn
               (:= tokens #h(0 +root+))
               (when deps (push (reverse deps) all-deps))
               (void deps))
             (push (read-stanford-dep line tokens) deps)))
      (reverse all-deps)))
  (:method ((format (eql :conll)) (str stream))
    (let ((tokens #h(0 +root+))
          all-deps deps)
      (loop :for line := (read-line str nil) :while line :do
         (if (blankp line)
             (progn
               (when deps
                 (dolist (dep deps)
                   (:= (dep-govr dep)
                       (? tokens (token-id (dep-govr dep)))))
                 (push (reverse deps) all-deps))
               (:= tokens #h(0 +root+))
               (void deps))
             (push (read-conll-dep line tokens) deps)))
      (reverse all-deps))))

(defun deps->tree (deps &optional dep)
  (when deps
    (unless dep (:= dep (find 'dep:root deps :key #'dep-rel)))
    (with-slots (dept) dep
      (cons dep
            (sort (cons dept
                        (mapcar #`(deps->tree deps %)
                                (remove-if-not #`(eql dept %) deps
                                               :key 'dep-govr)))
                  '< :key #`(token-id (etypecase %
                                        (token %)
                                        (list (dep-dept (first %))))))))))

#+nil
(defun pprint-deps-tree (deps)
  (let ((*package* (find-package :dep)))
    (pprint-tree (maptree #`(etypecase %
                              (dep (dep-rel %))
                              (token (token-word %)))
                          (deps->tree deps)))))
