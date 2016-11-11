;;; (c) 2013 Vsevolod Dyomkin

(in-package #:nlp.util)
(named-readtables:in-readtable rutils-readtable)


;;; Tree iteration

(defmacro dotree ((subtree tree &optional result) &body body)
  "Iterate over each SUBTREE of the TREE in depth-first order.
   Optionally return RESULT."
  (with-gensyms (rec child)
    `(labels ((,rec (,subtree)
                ,@body
                (unless (atom ,subtree)
                  (dolist (,child (cdr ,subtree))
                    (,rec ,child)))))
       (when-it ,tree
         (,rec it))
       ,result)))

(defmacro doleaves ((node tree &optional result) &body body)
  "Iterate over each leaf NODE of the TREE in depth-first order.
   Optionally return RESULT."
  (with-gensyms (rec child)
    `(labels ((,rec (,node)
                (unless (atom ,node)
                  (dolist (,child (cdr ,node))
                    (if (atom ,child)
                        (let ((,node ,child))
                          ,@body)
                        (,rec ,child))))))
       (when-it ,tree
         (,rec it))
       ,result)))

(defun maptree (fn tree)
  "Map a one-argument function FN over each NODE of the TREE
   in depth-first order, returning a new tree with the same structure."
  (labels ((rec (node)
             (if (atom node)
                 (funcall fn node)
                 (cons (funcall fn (car node))
                       (mapcar #'rec (cdr node))))))
    (when-it tree
      (rec it))))

(defun mapleaves (fn tree)
  "Map a one-argument function FN over each leaf NODE of the TREE
   in depth-first order, returning a new tree with the same structure."
  (labels ((rec (node)
             (if (atom node)
                 (funcall fn node)
                 (cons (car node)
                       (mapcar #'rec (cdr node))))))
    (when-it tree
      (rec it))))

;; (defun mapcantree (fn tree)
;;   "Mapcan a one-argument function FN over each NODE of the TREE
;;    in depth-first order, returning a new tree with the same structure."
;;   (labels ((rec (node)
;;              (if (atom node)
;;                  (when-it (funcall fn node)
;;                    (list it))
;;                  (if-it (funcall fn (car node))
;;                         (cons it (mapcan #'rec (cdr node)))
;;                         (mapcan #'rec (cdr node))))))
;;     (when-it tree
;;       (rec it))))
