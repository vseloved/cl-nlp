;;; (c) 2013-2014 Vsevolod Dyomkin

(in-package #:nlp.corpora)
(named-readtables:in-readtable rutils-readtable)


(defstruct (treebank-text (:include text))
  "A single text from the treebanked corpus."
  trees)


(defmethod read-corpus-file ((type (eql :treebank)) file)
  (let ((raw (string-trim +white-chars+ (read-file file)))
        (*package* (find-package :nlp.tags)))
    (with-input-from-string (in (prepare-tree-for-reading raw))
      (loop
         :for tree := (read in nil) :while tree
         ;; in some treebanks every tree is enclosed in an additional list
         :do (when (and (single tree)
                        (listp (first tree)))
               (setf tree (atomize tree)))
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
         ;; in OntoNotes there're huge texts (CNN section)
         ;; that just don't fit into memory, so excess data (raw)
         ;; that, most probably isn't going to be used anyway, is taken away
         :finally
         (return (values nil
                         (mapcar #`(strjoin #\Space (mapcar #'token-word %))
                                 tokens)
                         tokens
                         trees))))))

(defmethod read-corpus ((type (eql :treebank)) path &key ext)
  (let ((rez (make-corpus :desc "Treebank")))
    (walk-corpus-dir
     path ext
     #`(mv-bind (raw clean tokens trees) (read-corpus-file :treebank %)
         (push (make-treebank-text :name (pathname-name %)
                                   :raw raw :clean clean
                                   :tokens tokens :trees trees)
               (corpus-texts rez))))
    rez))

(defmethod map-corpus ((type (eql :treebank)) path fn &key ext)
  (walk-corpus-dir
   path ext
   #`(mv-bind (raw clean tokens trees) (read-corpus-file :treebank %)
       (format *debug-io* ".")
       (funcall fn (make-treebank-text :name (pathname-name %)
                                       :raw raw :clean clean
                                       :tokens tokens :trees trees)))))

;;; utils

(defun prepare-tree-for-reading (string)
  "Treebanked data is almost like Lisp code, but not quite.
   That's why this pre-processing is needed."
  (strjoin
   " "
   (mapcar #`(cond-it
               ((char= #\( (char % 0))
                (cond-it
                  ((member (slice % 1) '("." "," ";" ":" "#" "''" "``")
                           :test 'string=)
                   (fmt "(|~A|" (slice % 1)))
                  ((position #\| %)
                   (fmt "(|~A\\|~A|" (slice % 1 it) (slice % (1+ it))))
                  (t %)))
               ((position #\) %)
                (cond ((zerop it)
                       %)
                      ((and (< (1+ it) (length %))
                            (position #\( %))
                       (fmt "\"~A\")" (slice % 0
                                             (position #\) % :start (1+ it)))))
                      (t
                       (fmt "\"~A\"~A" (slice % 0 it) (slice % it)))))
               (t
                (fmt "\"~A\"" %)))
           (split-sequence-if #'white-char-p
                              (re:regex-replace-all "\"" string "\\\"")
                              :remove-empty-subseqs t))))

(defun remove-dummy-tokens (sent)
  "Remove tokens tagged as -NONE- from list of tokens SENT."
  (remove-if #`(eql 'tag:-NONE- (token-tag %))
             sent))

(defun clean-up-tree (tree)
  "Remove dummy tokens and corresponding subtrees from TREE.
   Also normalize special tag names (like 'NP-SUBJ') to a regular variant ('NP')."
  (let (rez)
    (if (stringp tree)
        tree
        (let ((head (first tree)))
          (unless (eql 'tag:-NONE- head)
            (when-let (children (remove nil (mapcar #'clean-up-tree (rest tree))))
              (cons (intern (slice (symbol-name head) 0
                                   (position #\- (symbol-name head)))
                            :plang)
                    children)))))))
