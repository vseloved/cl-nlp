;;; (c) 2013 Vsevolod Dyomkin

(in-package #:nlp.util)
(named-readtables:in-readtable rutils-readtable)


(defun test-file (filename)
  (merge-pathnames (strcat "test/" filename) +project-root+))


(defun test-on-corpus (func corpus expected &key (test 'equal))
  (let ((tp 0)
        (fp 0))
  ))

#+nil
(test-on-corpus #`(tokenize (make 'regex-word-tokenizer) %)
                +brown-corpus+
                (test-file "brown-tokenization.txt"))

(defpackage #:nlp.tests
  (:use))

(defmacro deftest (name () &body body)
  ""
  (with-gensyms (failed test expr expected got k v)
    `(defun ,(intern (symbol-name name) (find-package '#:nlp.tests)) ()
       (let ((,failed #{}))
         (macrolet ((test (test expr &optional (expected nil expected-p))
                      `(let ((,',got (handler-case ,expr
                                       (error (e) (type-of e)))))
                         (unless (apply ,test ,',got
                                        (when ,expected-p (list ,expected)))
                           (set# ',expr ,',failed (cons ,expected ,',got))))))
           ,@body
           (if (zerop (hash-table-count ,failed))
               t
               (dotable (,k ,v ,failed (values nil ,failed))
                 (format *debug-io* "~&+++ failed ~S: expected ~S - got ~S~%"
                         ,k (car ,v) (cdr ,v)))))))))

(defun run-tests (&rest names)
  ""
  (do-symbols (sym (find-package '#:nlp.tests))
    (when (or (null names)
              (member sym names))
      (format *debug-io* "TEST ~A " sym)
      (mv-bind (rez failed) (funcall sym)
        (apply #'format *debug-io*
               (if rez (list "-> OK~%")
                   (list "~%FAILED ~A tests~%" (hash-table-count failed))))))))