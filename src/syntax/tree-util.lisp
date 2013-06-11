;;; (c) 2013 Vsevolod Dyomkin

(in-package #:nlp.syntax)
(named-readtables:in-readtable rutils-readtable)


(defun proper-subtree-p (subtree)
  (if (atom subtree)
      (stringp subtree)
      (and (symbolp (car subtree))
           (or (single (cdr subtree))
               (every #'listp (cdr subtree))))))

(defmethod normalize ((form (eql 'cnf)) tree)
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
                (rec (cons (mksym (fmt "~A+~A" (car subtree) (caadr subtree)))
                           (cdadr subtree))))
               (t
                (cons (car subtree)
                      (mapcar #'rec (cdr subtree))))))
           (subrec (nodes)
             (assert (proper-subtree-p (cons nil nodes)) (nodes)
                     "Subtree ~S is improper!" nodes)
             (if (dyadic nodes)
                 (mapcar #'rec nodes)
                 (let ((left (butlast nodes)))
                   (cons (cons (mksym (fmt "~{~A~^_~}" (mapcar #'car left)))
                               (subrec left))
                         (list (rec (last1 nodes))))))))
    (when-it tree
      (rec it))))

(defmethod denormalize ((form (eql 'cnf)) tree)
  "Denormalize TREE from Chomsky Normal Form."
  (labels ((rec (subtree)
             (assert (proper-subtree-p subtree) (subtree)
                     "Subtree ~S is improper!" subtree)
             (cond
               ((atom subtree)
                (list subtree))
               ((find #\+ (symbol-name (car subtree)))
                (let ((parts (mapcar #'mksym (split
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

;; (defun format-connectors (len points)
;;   "Return a string of connectors between tree levels like this:
;;    ┌────┬───┴────┬───┐
;;    LEN is total string length, POINTS is a list of connector points positions."
;;   (assert (apply '< points) (points)
;;           "Points are not strictly growing: ~A" points)
;;   (assert (< (last1 points) len) (len points)
;;           "Length (~A) is less than points ~A" len points)
;;   (let ((center (floor len 2))
;;         (prev (car points)))
;;     (if (single points)
;;         (fmt "~A│~A"
;;              (filler (floor len 2))
;;              (filler (floor len 2)))
;;         (fmt "~A┌~A┐~A"
;;              (filler (car points))
;;              (if (= 2 (length points))
;;                  (strcat (filler (- center (car points) 1) #\─)
;;                          "┴"
;;                          (filler (- (cadr points) center 1) #\─))
;;                  (substr (with-output-to-string (out)
;;                            (dolist (point (cdr points))
;;                              (if (and (< prev center)
;;                                       (>= point center))
;;                                  (format out "~A~C~A~A"
;;                                          (filler (- center prev 1) #\─)
;;                                          (if (= point center) #\┼ #\┴)
;;                                          (filler (- point center 1) #\─)
;;                                          (if (= point center) "" "┬"))
;;                                  (format out "~A~C"
;;                                          (filler (- point prev) #\─) #\┬))
;;                              (setf prev point)))
;;                          0 -1))
;;              (filler (- len (last1 points)))))))

;; (defun pprint-tree (tree &optional (stream *standard-output*))
;;   "Pretty print TREE to STREAM."
;;   (labels ((rec (subtree)
;;              (if (atom subtree)
;;                  (cons (list subtree)
;;                        (length subtree))
;;                  (let ((len 0)
;;                        children
;;                        points)
;;                    (loop :for (child . child-len)
;;                          :in (mapcar #'rec (cdr subtree)) :do
;;                       (incf len (1+ child-len))
;;                       (push child children)
;;                       (push (floor len 2) points))
;;                    (when (evenp len)
;;                      (incf len))
;;                    (let* ((top-len (length (princ-to-string (car subtree))))
;;                           (empty-len (- len top-len))
;;                           (top-filler (if (plusp empty-len)
;;                                           (filler (floor empty-len 2))
;;                                           "")))
;;                      (cons (cons (fmt "~A~S~A"
;;                                       top-filler (car subtree) top-filler)
;;                                  (cons (format-connectors len (reverse points))
;;                                        (mapcar #`(fmt "~{ ~A~}" %)
;;                                                (apply #'zip
;;                                                       (reverse children)))))
;;                            len))))))
;;     (dolist (level (car (rec tree)))
;;       (format stream "~A~%" level))))