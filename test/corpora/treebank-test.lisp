;;; (c) 2013 Vsevolod Dyomkin

(in-package #:ncorpus)
(named-readtables:in-readtable rutils-readtable)
(use-package :should-test)

(deftest read-treebank ()
  (should be equal '((S (NP-SBJ "I")
                        (VP "do" "not"
                            (VP "mind"
                                (S (NP-SBJ "you(r)")
                                   (VP "leaving"
                                       (ADV-TMP "early")))))))
          (with-tmp-file (f "(S (NP-SBJ I)
                                (VP do not
                                    (VP mind
                                        (S (NP-SBJ you(r))
                                           (VP leaving
                                               (ADV-TMP early))))))")
            (read-corpus-file :treebank f))))


(defmacro with-tmp-file ((path contents) &body body)
  `(let ((,path (fmt "/tmp/cl-nlp-~A" (gensym))))
     (unwind-protect
          (progn (with-open-file (out ,path :direction :output
                                      :if-does-not-exist :create)
                   (princ ,contents out))
                 ,@body)
       (ignore-errors (delete-file ,path)))))
