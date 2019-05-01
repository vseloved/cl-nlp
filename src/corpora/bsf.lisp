;;; (c) 2017 Vsevolod Dyomkin

(in-package #:nlp.corpora)
(named-readtables:in-readtable rutilsx-readtable)


;;; utilities

(defparameter *basic-parag-splitter*
  (make 'parag-splitter :regex (re:create-scanner
                                (fmt "(?:~C|~C~C|~C|~C)"
                                     #\Newline #\Return #\Linefeed
                                     #\Return #\Linefeed))))
  
(defun tokenize-parag (str)
  (mapcar ^(make 'sent :toks (mapcar 'tok->ent @%.toks))
          (flatten (tokenize *full-text-tokenizer* str))))

(defun parse-bsf-line (line)
  (with (((_ cat-beg-end _) (split #\Tab line))
         ((cat beg end) (split #\Space cat-beg-end))
         (cat (intern cat :tag))
         ((beg end) (mapcar 'parse-integer (list beg end))))
    (list cat beg end)))


;;; API
  
(defmethod read-corpus-file ((type (eql :bsf)) file &key)
  "Read individual file with annotations from the corpus in BSF."
  (with ((ann-file  (merge-pathnames (fmt "~A.ann" (pathname-name file))
                                     (uiop:pathname-directory-pathname file)))
         (text (read-file file))
         (parags parag-spans (tokenize *basic-parag-splitter* text))
         (parag-sent-toks nil)
         (ann-line nil))
    (unless (probe-file ann-file)
      (warn "Missing annotation file ~A" ann-file)
      (return-from read-corpus-file nil))
    (block proc-ann-file
      (with-open-file (in ann-file :if-does-not-exist nil)
        (if (:= ann-line (and in (read-line in nil)))
            (with (((cat beg end) (parse-bsf-line ann-line))
                   (overlap nil))
              (loop :while (and parags ann-line) :do
                ;; adding paragraphs w/o annotation as is
                (loop :while (> beg (? parag-spans 0 1)) :do
                  (push (tokenize-parag @parags#0) parag-sent-toks)
                  (:= parag-spans (rest parag-spans)
                      parags (rest parags))
                  (unless parags
                    (warn "Annotation out of range in ~A: ~A" ann-file ann-line)
                    (return-from proc-ann-file)))
                ;; adding paragraph with annotation
                (with ((sents spans (tokenize <sent-splitter> @parags#0))
                       (cur nil)
                       (ner nil)
                       (parag-off (? parag-spans 0 0)))
                  (loop :for sent :in sents
                        :for (sb se) :in spans :do
                    (let ((toks (? (tokenize-parag sent) 0))
                          (off (+ sb parag-off)))
                      ;; adding NER info to tokens
                      (flet ((maybe-read-ann-line ()
                               (if (:= ann-line (and in (read-line in nil)))
                                   (with (((c b e) (parse-bsf-line ann-line)))
                                     (:= cat c
                                         beg b
                                         end e))
                                   (:= beg nil
                                       parag-spans (rest parag-spans)
                                       parags (rest parags)))))
                        (cond
                          ((or overlap (and beg (<= off beg)))
                           (dolist (tok @toks.toks
                                        (when ner (:= overlap t)))
                             (when (and beg (= beg (+ @tok.beg off)))
                               (:= ner cat))
                             (:= @tok.ner ner)
                             (when (and beg
                                        (or (= end (+ @tok.end off))
                                            (when (< end (+ @tok.end off))
                                              (warn "Annotation end mismatch in ~A: ~A"
                                                    ann-file ann-line)
                                              t)))
                               (void ner)
                               (void overlap)
                               (maybe-read-ann-line))))
                          (beg
                           (warn "Annotation beg mismatch in ~A: ~A"
                                 ann-file ann-line)
                           (maybe-read-ann-line))))
                      (push toks cur)))
                  (push (reverse cur) parag-sent-toks)
                  (when overlap
                    (:= parag-spans (rest parag-spans)
                        parags (rest parags)))))))))
    ;; adding trailing paragraphs
    (dolist (parag parags)
      (push (tokenize-parag parag) parag-sent-toks))
    (when ann-line
      (warn "Not all anns processed in ~A: ~A" ann-file ann-line))
    (reversef parag-sent-toks)
    (make-text :name (pathname-name file)
               :raw text
               :clean (parags->text parag-sent-toks)
               :parag-sent-toks parag-sent-toks)))

(defmethod read-corpus ((type (eql :bsf)) path &key (ext "txt"))
  (make-corpus :texts (remove nil
                              (mapcar ^(read-corpus-file :bsf %)
                                      (directory (fmt "~A*.~A" path ext))))))

(defmethod map-corpus ((type (eql :bsf)) path fn &key (ext "txt"))
  (dofiles (file path :ext ext)
    (when-it (read-corpus-file :bsf file)
      (call fn it))))
