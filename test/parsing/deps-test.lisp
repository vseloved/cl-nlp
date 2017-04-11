;;; (c) 2014-2015 Vsevolod Dyomkin

(in-package #:nlp.parsing)
(named-readtables:in-readtable rutils-readtable)


(deftest read-stanford-dep ()
  (should be equalp
         (make-dep :rel 'dep:nsubjpass
                   :head (make-tok :id 8 :word "submitted")
                   :child (make-tok :id 7 :word "Bills"))
         (read-stanford-dep "nsubjpass(submitted-8, Bills-7)"))
  (should be eql dep:+root+
         (dep-head (read-stanford-dep "root ( ROOT-0 , test-4 )"))))

(defun read-dep-from-string (str)
  (? (read-deps :conll str) 0 0))

(deftest read-conll-dep ()
  (should be equalp
         (make-dep :rel 'dep::NMOD
                   :child (make-tok :id 1 :word "Pricing"
                                   :lemma "pricing" :pos 'tag:NN)
                   :head (make-tok :id 2))
         (read-dep-from-string
          "1	Pricing	pricing	NN	_	_	2	NMOD"))
  (should be equalp
          (make-dep :rel 'dep::NMOD
                    :child (make-tok :id 1 :word "Pricing" :pos 'tag:NN)
                    :head (make-tok :id 2))
          (read-dep-from-string
           "1	Pricing	_	NN	_	_	2	NMOD")))
