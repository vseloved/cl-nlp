;;; (c) 2013-2015 Vsevolod Dyomkin

(in-package #:nlp.corpora)
(named-readtables:in-readtable rutils-readtable)


(defstruct (treebank-text (:include text) (:conc-name text-))
  "A single text from the treebanked corpus. Adds TREES slot."
  trees)


(defmethod read-corpus-file ((type (eql :treebank)) file &key)
  (let ((*package* (find-package :tag)))
    (with-input-from-string (in (prepare-tree-for-reading
                                 (string-trim +white-chars+ (read-file file))))
      (loop
         :for tree := (read in nil) :while tree
         ;; in some treebanks every tree is enclosed in an additional list
         :do (when (and (single tree)
                        (listp (first tree)))
               (:= tree (atomize tree)))
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
                                 :end (1- (:+ pos (1+ (length word))))
                                 :word word
                                 :tag (first subtree))
                                toks))))
                    (reverse toks))
         :into sents
         ;; in OntoNotes there're huge texts (CNN section)
         ;; that just don't fit into memory, so excess data (raw)
         ;; that, most probably isn't going to be used anyway, is not preserved
         :finally
         (return (make-treebank-text
                  :name (pathname-name file)
                  :clean (strjoin #\Newline
                                  (mapcar #`(strjoin #\Space
                                                     (mapcar #'token-word %))
                                          sents))
                  :tokenized (list sents)
                  :trees trees))))))

(defmethod read-corpus ((type (eql :treebank)) path &key ext)
  (let ((rez (make-corpus :desc "Treebank")))
    (dofiles (file path :ext ext)
      (push (read-corpus-file :treebank file) (corpus-texts rez)))
    rez))

(defmethod map-corpus ((type (eql :treebank)) path fn &key ext)
  (dofiles (file path :ext ext)
    (funcall fn (read-corpus-file :treebank file))))


;;; utils

(defun prepare-tree-for-reading (string)
  "Treebanked data is almost like Lisp code, but not quite.
   That's why this pre-processing is needed."
  (strjoin
   #\Space
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
              (let ((headstr (symbol-name head)))
              (cons (intern (slice headstr 0 (position #\- headstr)) :tag)
                    children))))))))
