;;; (c) 2013 Vsevolod Dyomkin

(in-package #:ncorp)
(named-readtables:in-readtable rutils-readtable)

(defmacro with-tmp-file ((path contents) &body body)
  `(let ((,path (fmt "/tmp/cl-nlp-~A" (gensym))))
     (unwind-protect
          (progn (with-open-file (out ,path :direction :output
                                      :if-does-not-exist :create)
                   (princ ,contents out))
                 ,@body)
       (ignore-errors (delete-file ,path)))))


(deftest read-treebank ()
  (should be equal '((S (NLP.TAGS::NP-SBJ "I")
                        (VP "do" "not"
                            (VP "mind"
                                (S (NLP.TAGS::NP-SBJ "you(r)")
                                   (VP "leaving"
                                       (NLP.TAGS::ADV-TMP "early")))))))
          (with-tmp-file (f "(S (NP-SBJ I)
                                (VP do not
                                    (VP mind
                                        (S (NP-SBJ you(r))
                                           (VP leaving
                                               (ADV-TMP early))))))")
	    (mv-bind (nil? tokens-str tokens trees)
		(read-corpus-file :treebank f)
	      trees))))
