;;; (c) 2013-2017 Vsevolod Dyomkin

(in-package #:nlp.parsing)
(named-readtables:in-readtable rutilsx-readtable)


(defun proper-subtree-p (subtree)
  (if (atom subtree)
      (stringp subtree)
      (and (symbolp (first subtree))
           (or (single (rest subtree))
               (every 'listp (rest subtree))))))

(defmethod normalize ((form (eql 'chomsky-nf)) tree)
  "Normalize TREE to Chomsky Normal Form."
  (labels ((rec (subtree)
             (assert (proper-subtree-p subtree) (subtree)
                     "Subtree ~S is improper!" subtree)
             (cond
               ((atom subtree)
                subtree)
               ((cdddr subtree)
                (cons (first subtree)
                      (subrec (rest subtree))))
               ((and (single (rest subtree))
                     (listp (second subtree)))
                (let ((*package* (find-package :nlp.parsing)))
                  (rec (cons (mksym (fmt "~A+~A" (car subtree) (caadr subtree)))
                             (cdadr subtree)))))
               (t
                (cons (first subtree)
                      (mapcar 'rec (rest subtree))))))
           (subrec (nodes)
             (assert (proper-subtree-p (cons nil nodes)) (nodes)
                     "Subtree ~S is improper!" nodes)
             (if (dyadic nodes)
                 (mapcar 'rec nodes)
                 (let ((left (butlast nodes))
                       (*package* (find-package :nlp.parsing)))
                   (cons (cons (mksym (fmt "~{~A~^_~}" (mapcar 'first left)))
                               (subrec left))
                         (list (rec (last1 nodes))))))))
    (when-it tree
      (rec it))))

(defmethod denormalize ((form (eql 'chomsky-nf)) tree)
  "Denormalize TREE from Chomsky Normal Form."
  (labels ((rec (subtree)
             (assert (proper-subtree-p subtree) (subtree)
                     "Subtree ~S is improper!" subtree)
             (cond
               ((atom subtree)
                (list subtree))
               ((find #\+ (symbol-name (first subtree)))
                (with ((*package* (find-package :nlp.parsing))
                       (parts (mapcar 'mksym (split #\+
                                                    (symbol-name (first subtree))))))
                  (list (reduce 'list (butlast parts)
                                :from-end t
                                :initial-value
                                (cons (last1 parts)
                                      (flat-map 'rec (rest subtree)))))))
               ((find #\_ (symbol-name (first subtree)))
                (flat-map 'rec (rest subtree)))
               (t
                (list (cons (first subtree)
                            (flat-map 'rec (rest subtree))))))))
    (when-it tree
      (car (rec it)))))
