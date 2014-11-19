;;; (c) 2013 Vsevolod Dyomkin

(in-package #:nlp.parsing)
(named-readtables:in-readtable rutils-readtable)


(defun proper-subtree-p (subtree)
  (if (atom subtree)
      (stringp subtree)
      (and (symbolp (car subtree))
           (or (single (cdr subtree))
               (every #'listp (cdr subtree))))))

(defmethod normalize ((form (eql 'chomsky-nf)) tree)
  "Normalize TREE to Chomsky Normal Form."
  (labels ((rec (subtree)
             (assert (proper-subtree-p subtree) (subtree)
                     "Subtree ~S is improper!" subtree)
             (cond
               ((atom subtree)
                subtree)
               ((cdddr subtree)
                (cons (car subtree)
                      (subrec (cdr subtree))))
               ((and (single (rest subtree))
                     (listp (second subtree)))
                (let ((*package* (find-package :nlp.parsing)))
                  (rec (cons (mksym (fmt "~A+~A" (car subtree) (caadr subtree)))
                             (cdadr subtree)))))
               (t
                (cons (car subtree)
                      (mapcar #'rec (cdr subtree))))))
           (subrec (nodes)
             (assert (proper-subtree-p (cons nil nodes)) (nodes)
                     "Subtree ~S is improper!" nodes)
             (if (dyadic nodes)
                 (mapcar #'rec nodes)
                 (let ((left (butlast nodes))
                       (*package* (find-package :nlp.parsing)))
                   (cons (cons (mksym (fmt "~{~A~^_~}" (mapcar #'car left)))
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
               ((find #\+ (symbol-name (car subtree)))
                (let* ((*package* (find-package :nlp.parsing))
                       (parts (mapcar #'mksym (split
                                               #\+ (symbol-name (car subtree))))))
                  (list (reduce #'list (butlast parts)
                                :from-end t
                                :initial-value
                                (cons (last1 parts)
                                      (mapcan #'rec (cdr subtree)))))))
               ((find #\_ (symbol-name (car subtree)))
                (mapcan #'rec (cdr subtree)))
               (t
                (list (cons (car subtree)
                            (mapcan #'rec (cdr subtree))))))))
    (when-it tree
      (car (rec it)))))
