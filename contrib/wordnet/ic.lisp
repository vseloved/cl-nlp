;;; (c) 2013 Vsevolod Dyomkin

(in-package #:nlp.contrib.wordnet)
(named-readtables:in-readtable rutils-readtable)


(defvar *ic* nil
  "Information content mapping.")

(defun load-ic (&key (path (merge-pathnames "data/wordnet_ic.zip"
                                            (asdf:system-definition-pathname 'cl-nlp)))
                     (filename-pattern "ic-[a-z]+")
                     (initial-value 0))
  (let ((rez (make-hash-table :test 'equal))
        (synset-ids (query (select '(synsetid pos :from synsets)))))
    (loop :for (sid pos) :in synset-ids :do
       (set# (cons (- sid 100000000) pos) rez initial-value))
    (zip:with-zipfile (zip path)
      (zip:do-zipfile-entries (name entry zip)
        (when (and (ends-with "dat" name)
                   (re:scan (fmt "~A\\." filename-pattern) name))
          (dolist (line (rest (split #\Newline
                                     (flex:octets-to-string
                                      (zip:zipfile-entry-contents entry)
                                      :external-format
                                      '(:utf-8 :eol-style :crlf)))))
            (unless (blankp line)
              (ds-bind (id count &optional root) (split #\Space line)
                (let ((sid (parse-integer (substr id 0 -1)))
                      (pos (char id (1- (length id)))))
                  (set# (cons sid pos) rez
                        (+ (get# (cons sid pos) rez 0)
                           (parse-integer count))))))))))
    rez))

(defun information-content (synset)
  (unless *ic*
    (setf *ic* (load-ic)))
  (- (log (/ (get# (cons (- (synset-id synset) 100000000) (synset-pos synset))
                   *ic* 0)
             (get# (cons 1740 (synset-pos synset)) *ic*)))))
