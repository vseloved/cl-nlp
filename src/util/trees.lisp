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
                 node
                 (cons (car node)
                       (mapcar #`(if (atom %)
                                     (funcall fn %)
                                     (rec %))
                               (cdr node))))))
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


;;; Tree pretty printing.

(defun pprint-tree (tree &optional (stream *standard-output*))
  "Pretty print TREE represented as a list to STREAM.

   The result may look like this:

   NLP> (pprint-tree '(\"S\" (\"NP\" (\"NP\" \"Theeeee\" \"big\" \"b\")
                                     \"test\")
                             (\"VP\" (\"PP\" \"sea\" 1 2 (3 5 6)
                                             4 (5 aa) 6))))
                            S
              /~~~~~~~~~~~~~~~~~~~~~~~~~\
              NP                        VP
           /~~~~~~~~~~~\                |
           NP         test              PP
       /~~~~~~|~~~\         /~~~|~~|~~~~|~~~|~~~|~~\
    Theeeee  big  b        sea  1  2    3   4   5  6
                                      /~~\      |
                                      5  6     AA
  "
  (labels ((to-strings (subtree)
             (let ((head (fmt " ~A " (atomize subtree))))
               (if (atom subtree) (list head)
                   (let* ((children (mapcar #'to-strings (cdr subtree)))
                          (merged (apply #'zip-with #'strcat
                                         (align-bottoms children)))
                          (merged+ (cons (format-connectors (car merged))
                                         merged))
                          (diff (- (length head) (length (car merged)))))
                     (if (plusp diff)
                         (cons head (mapcar #`(pad diff %) merged+))
                         (cons (pad (- diff) head) merged+)))))))
    (dolist (level (to-strings tree))
      (write-line level stream))))

(defun format-connectors (string)
  "Print connectors above one level of the tree being pprinted."
  (let* ((width (length string))
         (center (floor width 2))
         (spans (re:all-matches "[^\\s]+" string)))
    (if (= 2 (length spans))  ; spans = (1 2) for string = " I "
        (strcat (filler center)
                "|"
                (filler (- width center 1)))
        (let ((pos (ceiling (+ (first spans) (second spans)) 2)))
          (fmt "~A/~{~A~}~A"
               (filler (1- pos))
               (loop
                  :for (beg end . tail) :on (cddr spans) :by #'cddr
                  :for c := (ceiling (+ beg end) 2)
                  :collect (strcat (filler (- c pos 1) #\~)
                                   (if tail "|" "\\"))
                  :do (setf pos c)) ;(print (list beg end c)) (force-output))
               (filler (- width pos)))))))

(defun align-bottoms (string-stacks)
  "Align STRING-STACKS list to the same number of layers in each stack
   by adding if necessary layers at the bottom of the same width
   as all other layers of this stack."
  (let ((max-depth (apply #'max (mapcar #'length string-stacks))))
    (mapcar #`(let ((spaces (filler (length (last1 %)))))
                (do ((d (length %) (1+ d)))
                    ((= d max-depth) %)
                  (setf % (append % (list spaces)))))
            string-stacks)))

(defun pad (padding text)
  "Return a string of TEXT padded with PADDING spaces devided equally
   on both sides of it."
  (strcat (filler (ceiling padding 2))
          text
          (filler (floor padding 2))))