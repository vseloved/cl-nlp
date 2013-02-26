;;; (c) 2013 Vsevolod Dyomkin

(in-package #:nlp.util)
(named-readtables:in-readtable rutils-readtable)


;;; Some package renaming

(rename-package "CL-PPCRE" "CL-PPCRE" '("PPCRE" "RE"))


;;; Conditions

(define-condition nlp-error (simple-error) ())

(define-condition not-implemented-error (simple-error) ())


;;; Working with chars

(defparameter +newline+
  (fmt "~%")
  "A string with a single newline.")

(defparameter +white-chars+
  '(#\Space #\Tab #\Newline #\Return #\Linefeed)
  "Chars considered WHITESPACE.")

(defun white-char-p (char)
  "Test if CHAR is in +WHITE-CHARS+."
  (member char +white-chars+))

(defparameter +period-chars+
  '(#\. #\? #\!)
  "Chars considered legitimate sentence endings.")

(defun period-char-p (char)
  "Test if CHAR is in +PERIOD-CHARS+."
  (member char +period-chars+))

(defparameter +newline-chars+
  '(#\Newline #\Return #\Linefeed)
  "Chars considered legitimate paragraph endings.")

(defun newline-char-p (char)
  "Test if CHAR is in +PERIOD-CHARS+."
  (member char +newline-chars+))

(declaim (inline filler))
(defun filler (n &optional (fill-char #\Space))
  "Produce an N-element filler string of FILL-CHAR's."
  (make-string n :initial-element fill-char))


;;; Working wtih words

(defparameter *stopwords-en*
  '("!" "\"" "'" "," "-" "." ":" ";" "</S>" "<S>" "?" "a" "about" "above" "after" "again" "against" "all" "am" "an" "and" "any" "are" "aren't" "as" "at" "be" "because" "been" "before" "being" "below" "between" "both" "but" "by" "can't" "cannot" "could" "couldn't" "d" "did" "didn't" "do" "does" "doesn't" "doing" "don't" "down" "during" "each" "few" "for" "from" "further" "had" "hadn't" "has" "hasn't" "have" "haven't" "having" "he" "he'd" "he'll" "he's" "her" "here" "here's" "hers" "herself" "him" "himself" "his" "how" "how's" "i" "i'd" "i'll" "i'm" "i've" "if" "in" "into" "is" "isn't" "it" "it's" "its" "itself" "let" "let's" "ll" "me" "more" "most" "mustn't" "my" "myself" "n't" "no" "nor" "not" "of" "off" "on" "once" "only" "or" "other" "ought" "our" "ours " "ourselves" "out" "over" "own" "s" "same" "shan't" "she" "she'd" "she'll" "she's" "should" "shouldn't" "so" "some" "such" "t" "than" "that" "that's" "the" "their" "theirs" "them" "themselves" "then" "there" "there's" "these" "they" "they'd" "they'll" "they're" "they've" "this" "those" "through" "to" "too" "under" "until" "up" "very" "was" "wasn't" "we" "we'd" "we'll" "we're" "we've" "were" "weren't" "what" "what's" "when" "when's" "where" "where's" "which" "while" "who" "who's" "whom" "why" "why's" "with" "won't" "would" "wouldn't" "you" "you'd" "you'll" "you're" "you've" "your" "yours" "yourself" "yourselves")
  "List of english stopwords.")

(defun ending-word-p (word)
  "Check if string WORD is some kind of a period char or a paragraph mark."
  (or (every #'period-char-p word)
      (string= "¶" word)))


;;; Working with project files

(eval-always
  (defparameter +project-root+ (asdf:system-relative-pathname 'cl-nlp "")
    "Base dir of cl-nlp project."))

(defun data-file (filename)
  "File in data/ subdir of cl-nlp."
  (merge-pathnames (strcat "data/" filename)
                   +project-root+))

(defun list-from-file (file)
  "Load the contents of FILE into a list of strings for each trimmed line."
  (let (rez)
    (dolines (line file)
      (push (string-trim +white-chars+ line) rez))
    (reverse rez)))


;;; Search

(defun bin-search (val vec test-less &key (start 0) end key test)
  "Binary search for VAL in sorted vector VEC (the order property isn't checked).
   Needs to specify TEST-LESS predicate. Handles START, END, KEY as usual.
   It TEST is provided tests the value at the found position against VAL,
   and returns nil if it returns nil."
  (let ((low start)
        (high (or end (1- (length vec)))))
    (do ()
        ((= low high) (when (or (null test)
                                (funcall test val (svref vec high)))
                        (elt vec high)))
      (let ((mid (floor (+ low high) 2)))
        (if (funcall test-less (if key
                                   (funcall key (svref vec mid))
                                   (svref vec mid))
                     val)
            (setf low (1+ mid))
            (setf high mid))))))


;;; Misc

(defmacro define-lazy-singleton (name init &optional docstring)
  "Define a function NAME, that will return a singleton object,
   initialized lazily with INIT on first call.
   Also define a symbol macro <NAME> that will expand to (NAME)."
  (with-gensyms (singleton)
    `(let (,singleton)
       (defun ,name ()
         ,docstring
         (or ,singleton
             (setf ,singleton ,init)))
       (define-symbol-macro ,(mksym name :format "<~A>") (,name)))))

(defun sorted-ht-keys (test ht)
  "Return hash-table keys of HT in sorted order accroding to TEST."
  (sort (ht-keys ht) test :key #`(get# % ht)))

(defun shorter? (list n)
  "Tests if LIST has at least N elements."
  (let ((tail list))
    (loop :repeat (1- n) :do (setf tail (cdr tail)))
    (null tail)))

