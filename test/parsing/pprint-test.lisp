;;; (c) 2013-2017 Vsevolod Dyomkin

(in-package #:nlp.parsing)
(named-readtables:in-readtable rutilsx:rutilsx-readtable)

(defun toks (words)
  (let ((offset 0)
        (id 0))
    (mapcar ^(with (((word &optional tag) (split #\_ %)))
               (prog1 (make-tok
                       :word word
                       :pos (when tag (mksym tag :package 'tag))
                       :beg offset
                       :end (+ offset (length word))
                       :id id)
                 (:+ offset (1+ (length word)))
                 (:+ id)))
            words)))


(defun deps (sent deps)
  (mapcar ^(make-dep
            :rel (mksym (first %) :package 'dep)
            :head (if (= -1 (second %)) dep:+root+ (? sent (second %)))
            :child (? sent (third %)))
          deps))

(defparameter *test-sent0*
    (with ((sent (toks '("Hello" "world")))
           ((w-hello w-word) sent))
      (list sent
            `(TOP (NP (INTJ ,w-hello)
                      (NN ,w-word)))
            (deps sent '((root -1 0)
                         (npadvmod -0 1))))))

(defparameter *test-sent1*
  (with ((sent (toks '("This" "is" "a" "simple" "test" ".")))
         ((w-this w-is w-a w-simple w-test |w-.|) sent))
    (list sent
          `(TOP (S (NP (NN ,w-this))
                   (VP (VBZ ,w-is)
                       (NP (DT ,w-a)
                           (JJ ,w-simple)
                           (NN ,w-test)))
                   (|.| ,|w-.|)))
          (deps sent '((root -1 1)
                       (nsubj 1 0)
                       (dobj 1 4)
                       (det 4 2)
                       (amod 4 3)
                       (punct 1 5))))))

(deftest pprint-tags ()
  (with-output-to-string (*test-output*)
    (should print-to *test-output*
            " INTJ   NN  
Hello world "
            (pprint-tags (? *test-sent0* 1) :stream *test-output*))
    (should print-to *test-output*
            " NN  VBZ DT   JJ    NN  . 
This  is  a simple test . "
            (pprint-tags (? *test-sent1* 1) :stream *test-output*))))

(deftest pprint-tree ()
  (with-output-to-string (*test-output*)
    (should print-to *test-output*
        "    TOP     
     :      
     NP     
  .-----.   
 INTJ   NN  
  :     :   
Hello world "
        (pprint-tree (? *test-sent0* 1) :stream *test-output*))
    (should print-to *test-output*
            "           TOP            
            :             
            S             
  .-----------:---------. 
  :          VP         : 
  :   .---------.       : 
 NP   :        NP       : 
  :   :   .----:-----.  : 
 NN  VBZ DT   JJ    NN  . 
  :   :   :    :     :  : 
This  is  a simple test . "
            (pprint-tree (? *test-sent1* 1) :stream *test-output*))))

(deftest pprint-deps ()
  (with-output-to-string (*test-output*)
    (should print-to *test-output*
            "Hello         world 
  ^.            ^
  :`. npadvmod .´
 root"
            (pprint-deps (? *test-sent0* 2) :stream *test-output*))
    (should print-to *test-output*
            "This        is  a simple     test . 
  ^         .^. ^    ^        .^  ^
  `. nsubj .´:: :    `. amod .´:  :
             :: `.... det ....´:  :
             :`..... dobj .....´  :
             :`...... punct ......´
           root"
            (pprint-deps (? *test-sent1* 2) :stream *test-output*))))
