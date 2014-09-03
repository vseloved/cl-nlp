;;; (c) 2013 Vsevolod Dyomkin

(in-package #:nparse)
(named-readtables:in-readtable rutils-readtable)

(deftest normalize-to-chomsky-nf ()
  (should be null
          (normalize 'chomsky-nf '()))
  (should be equal '(A "a")
          (normalize 'chomsky-nf '(A "a")))
  (should be equal '(A+B "b")
          (normalize 'chomsky-nf '(A (B "b"))))
  (should be equal '(A (B "b") (C "c"))
          (normalize 'chomsky-nf '(A (B "b") (C "c"))))
  (should be equal '(A (B "b")
                       (C (D "d")
                          (E (G "g")
                             (H "h"))))
          (normalize 'chomsky-nf '(A (B "b")
                                     (C (D "d")
                                        (E (G "g")
                                           (H "h"))))))
  (should be equal '(A (B_C_E (B_C (B "b")
                                   (C+D "d"))
                              (E "e"))
                       (F (G "g")
                          (H "h")))
          (normalize 'chomsky-nf '(A (B "b")
                                     (C (D "d"))
                                     (E "e")
                                     (F (G "g")
                                        (H "h")))))
  (should be equal '(A (B "b")
                       (C (D "d")
                          (E+F (G_H (G "g")
                                    (H "h"))
                               (I+K "k"))))
          (normalize 'chomsky-nf '(A (B "b")
                                     (C (D "d")
                                        (E (F (G "g")
                                              (H "h")
                                              (I (K "k"))))))))
  (should signal simple-error
          (normalize 'chomsky-nf '(A (B "b")
                                     "C"
                                     (E "e")
                                     (F (G "g")
                                        (H "h")))))
  (should signal simple-error
          (normalize 'chomsky-nf '(A (B "b")
                                     ("C" "c")
                                     (E "e")
                                     (F (G "g")
                                        (H "h"))))))

(deftest denormalize-from-chomsky-nf ()
  (should be null
          (denormalize 'chomsky-nf '()))
  (should be equal '(A "a")
          (denormalize 'chomsky-nf '(A "a")))
  (should be equal '(A (B "b"))
          (denormalize 'chomsky-nf '(A+B "b")))
  (should be equal '(A (B "b") (C "c"))
          (denormalize 'chomsky-nf '(A (B "b") (C "c"))))
  (should be equal '(A (B "b")
                       (C (D "d")
                          (E (G "g")
                             (H "h"))))
          (denormalize 'chomsky-nf '(A (B "b")
                                       (C (D "d")
                                          (E (G "g")
                                             (H "h"))))))
  (should be equal '(A (B "b")
                       (C (D "d"))
                       (E "e")
                       (F (G "g")
                          (H "h")))
          (denormalize 'chomsky-nf '(A (B_C_E (B_C (B "b")
                                                   (C+D "d"))
                                              (E "e"))
                                       (F (G "g")
                                          (H "h")))))
  (should be equal '(A (B "b")
                       (C (D "d")
                          (E (F (G "g")
                                (H "h")
                                (I (K "k"))))))
          (denormalize 'chomsky-nf '(A (B "b")
                                       (C (D "d")
                                          (E+F (G_H (G "g")
                                                    (H "h"))
                                               (I+K "k"))))))
  (should signal simple-error
          (denormalize 'chomsky-nf '(A (B "b")
                                       "C"
                                       (E "e")
                                       (F (G "g")
                                          (H "h")))))
  (should signal simple-error
          (denormalize 'chomsky-nf '(A (B "b")
                                       ("C" "c")
                                       (E "e")
                                       (F (G "g")
                                          (H "h"))))))


;; NLP> (pprint-tree '(S (NP-SBJ-2 (PRP I))
;;                     (VP (VBP "'ve")
;;                      (ADVP-TMP (RB "always"))
;;                      (VP (VBN "wanted")
;;                       (S (VP (TO "to")
;;                              (VP (VB "ask")
;;                                  (NP (PRP "you"))
;;                                  (NP (DT "a")
;;                                      (NN "question")))))))))
;;                               S
;;     /^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^\
;;  NP-SBJ-2                          VP
;;     |        /^^^^^^|^^^^^^^^^^^^^^^^^^^^^^\
;;    PRP     VBP  ADVP-TMP                  VP
;;     |        |      |         /^^^^^^^^^^^^^^^^\
;;     I      've     RB       VBN                S
;;                     |         |                |
;;                  always   wanted              VP
;;                                   /^^^^^^^^^^^^^\
;;                                   TO            VP
;;                                    |    /^^^^|^^^^^^^^\
;;                                   to   VB   NP       NP
;;                                        |     |    /^^^^^^\
;;                                       ask  PRP  DT      NN
;;                                             |    |       |
;;                                            you   a  question


;; UTIL> (pprint-tree '(S (NP-SBJ-2 (PRP I))
;;                (VP (VBP (S (NP-SBJ (TO "tp") (VP "y")) (ADJP "'ve")))
;;                    (ADVP-TMP (RB "always"))
;;                    (VP (VBN "wanted")
;;                        (S NP-SBJ
;;                           (VP (TO "to")
;;                               (VP (VB "ask")
;;                                   (NP (PRP "you"))
;;                                   (NP (DT "a")
;;                                       (NN "question")))))))))
;;                                        S
;;     /~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~\
;;  NP-SBJ-2                                  VP
;;      |           /~~~~~~~~~~|~~~~~~~~~~~~~~~~~~~~~~~~~~\
;;     PRP         VBP      ADVP-TMP                      VP
;;      |           |           |        /~~~~~~~~~~~~~~~~~~~~~\
;;      I           S          RB       VBN                    S
;;              /~~~~~~\        |        |      /~~~~~~~~~~~~~~~~~\
;;            NP-SBJ  ADJP   always   wanted  NP-SBJ              VP
;;            /~~~\     |                             /~~~~~~~~~~~~~\
;;            TO  VP   've                            TO            VP
;;             |   |                                   |   /~~~~|~~~~~~~~\
;;            tp   y                                  to   VB   NP       NP
;;                                                         |    |   /~~~~~~\
;;                                                        ask  PRP  DT     NN
;;                                                              |    |      |
;;                                                             you   a  question
;; NIL
