;;; (c) 2013 Vsevolod Dyomkin

(cl:in-package #:cnlang)
(named-readtables:in-readtable rutils-readtable)


(defparameter +rare-word+ "_rare_")

(defun p1 (infile outfile)
  (let ((dict #{equal})
        (prominent-words #{equal})
        (rare-words #{equal}))
    (dolines (line infile)
      (doleaves (node (json:decode-json-from-string line))
        (incf (get# node dict 0))))
    (dotable (k v dict)
      (if (< v 5)
          (set# k rare-words v)
          (set# k prominent-words v)))
    (with-out-file (out outfile)
      (dolines (line infile)
        (json:encode-json (mapleaves #`(if (get# % rare-words)
                                           +rare-word+
                                           %)
                                     (json:decode-json-from-string line))
                          out)
        (terpri out)))
    prominent-words))

(defun read-rules (file)
  (let ((rules (make-hash-table :test 'equal)))
    (dolines (line file)
      (ds-bind (count type &rest rule) (split #\Space line)
        (setf (car rule) (mksym (car rule)))
        (when (eql :binaryrule (mkeyw type))
          (setf (second rule) (mksym (second rule))
                (third rule) (mksym (third rule))))
        (set# (if (single rule) (car rule) rule)
              rules (parse-integer count))))
    (dotable (rule count rules)
      (when (listp rule)
        (set# rule rules (/ count (get# (car rule) rules)))))
    (dotable (rule _ rules)
      (declare (ignore _))
      (unless (listp rule)
        (rem# rule rules)))
    rules))

(defun pcfg-from-rule-counts-file (file)
  (let* ((rules (read-rules file)))
    (make 'pcfg
          :rules rules
          :terminals (remove-duplicates (mapcan #`(when (dyadic %)
                                                    (copy-list (cdr %)))
                                                (ht-keys rules))
                                        :test 'string=)
          :nonterminals (cons 'TOP
                              (remove-duplicates (mapcar #'car (ht-keys rules))))
          :root 'TOP
          :root-rules '(;; ((TOP S) . 1.0)
                        ;; ((TOP SBAR) . 1.0)
                        ((TOP SBARQ) . 1.0)))))

(defparameter *prominent-words* (p1 "/mnt/ext4/nlp/t2/parse_train.dat"
                                    "/mnt/ext4/nlp/t2/parse_train.out"))

(defun parse-file (grammar infile outfile)
  (with-out-file (out outfile)
    (dolines (line infile)
      (print line)
      (let* ((words (split #\Space (string-trim " " line)))
             (tree (cadr ;; (car (parse-n grammar
                    (parse grammar
                                (substitute-if-not +rare-word+
                                                   #`(get# % *prominent-words*)
                                                   words))))
                                ;; 2))))
             (i -1))
        (json:encode-json (maptree #`(if (symbolp %) (fmt "~:@(~A~)" %) %)
                                   (mapleaves #`(nth (incf i) words)
                                              tree))
                          out)
        (terpri out)))))

(defun p2 (grammar infile outfile)
  (parse-file grammar infile outfile))

(defun p3 (grammar infile outfile)
  (parse-file grammar infile outfile))
