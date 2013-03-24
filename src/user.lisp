;;; (c) 2013 Vsevolod Dyomkin

(in-package #:nlp-user)
(named-readtables:in-readtable rutils-readtable)


(defun grep (word string &key (width 25) pass-newlines limit)
  "Print all (or LIMIT) WORD occurances in STRING with the surrounding
   context up to WIDTH chars. If PASS-NEWLINES isn't set, the context
   will be shown up to the closest newline."
  (declare (type (integer 0) width))
  (let* ((regex (re:create-scanner (fmt "\\b~A\\b" word)
                                   :case-insensitive-mode t))
         (matches (re:all-matches regex string))
         (len (/ (length matches) 2)))
    (format t "Displaying ~A of ~A matches~%" (if limit (min limit len) len) len)
    (loop :for (s e) :on matches :by #'cddr :do
       (let ((s- (- s width))
             (e+ (+ e width)))
         (if pass-newlines
             (format t "~A~%" (substitute-if #\Space #'white-char-p
                                             (subseq string s- e+)))
             (let ((l-pos (max s- (1+ (position #\Newline string
                                                :end s :from-end t))))
                   (r-pos (min e+ (1- (position #\Newline string :start e)))))
               (format t "~@[~A~]~A~@[~A~]~%"
                       (unless (= s- l-pos) (filler (- l-pos s-)))
                       (subseq string l-pos r-pos)
                       (unless (= e+ r-pos) (filler (- e+ r-pos))))))))))
