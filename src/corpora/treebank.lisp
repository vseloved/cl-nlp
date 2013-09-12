;;; (c) 2013 Vsevolod Dyomkin

(in-package #:nlp.corpora)
(named-readtables:in-readtable rutils-readtable)


(defstruct (treebank-text (:include text))
  "A single text from the (Penn) Treebank corpus."
  trees)


(defmethod read-corpus ((type (eql :treebank)) path)
  (let ((rez (make-corpus :desc "Treebank")))
    (fad:walk-directory
     path
     #`(mv-bind (raw clean tokens trees) (read-corpus-file :treebank %)
         (push (make-treebank-text :name (pathname-name %)
                                   :raw raw :clean clean
                                   :tokens tokens :trees trees)
               (corpus-texts rez))))
    rez))

(defmethod read-corpus-file ((type (eql :treebank)) file)
  (let ((raw (string-trim +white-chars+ (read-file file)))
        (*package* (find-package :ncorpus)))
    (with-input-from-string (in (prepare-tree-for-reading raw))
      (loop
         :for tree := (read in nil) :while tree
         :collect raw :into raws
         :collect tree :into trees
         :collect (let ((pos 0)
                        toks)
                    (dotree (subtree tree)
                      (when (and (listp subtree)
                                 (single (cdr subtree))
                                 (atom (cadr subtree)))
                        (let ((word (second subtree)))
                          (push (make-token
                                 :beg pos
                                 :end (1- (incf pos (1+ (length word))))
                                 :word word
                                 :tag (first subtree))
                                toks))))
                    (reverse toks))
         :into tokens
         :finally
         (return (values (strjoin #\Newline raws)
                         (mapcar #`(strjoin #\Space (mapcar #'token-word %))
                                 tokens)
                         tokens
                         trees))))))

(defmethod map-corpus ((type (eql :treebank)) path fn)
  (fad:walk-directory
   path
   #`(mv-bind (raw clean tokens trees) (read-corpus-file :treebank %)
       (funcall fn (make-treebank-text :name (pathname-name %)
                                       :raw raw :clean clean
                                       :tokens tokens :trees trees)))))

;;; utils

(defun prepare-tree-for-reading (string)
  (strjoin " " (mapcar #`(cond-it
                           ((char= #\( (char % 0))
                            (cond-it
                              ((member (sub % 1) '("." "," ";" ":" "#" "''" "``")
                                       :test 'string=)
                               (fmt "(|~A|" (sub % 1)))
                              ((position #\| %)
                               (fmt "(|~A\\|~A|" (sub % 1 it) (sub % (1+ it))))
                              (t %)))
                           ((position #\) %)
                            (cond ((zerop it) %)
                                  ((and (< (1+ it) (length %))
                                        (position #\( %))
                                   (fmt "\"~A\")"
                                        (sub % 0 (position #\) %
                                                           :start (1+ it)))))
                                  (t (fmt "\"~A\"~A" (sub % 0 it) (sub % it)))))
                           (t (fmt "\"~A\"" %)))
                       (split-sequence-if #`(member % +white-chars+)
                                          string :remove-empty-subseqs t))))
