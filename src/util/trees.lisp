;;; (c) 2013 Vsevolod Dyomkin

(in-package #:nlp.util)
(named-readtables:in-readtable rutils-readtable)


;;; Tree pretty printing.

(defun pprint-tree (tree &optional (stream *standard-output*))
  "Pretty print TREE represented as a list to STREAM.

   The result may look like this:

   CL-USER> (pprint-parse-tree '(\"S\" (\"NP\" (\"NP\" \"Theeeee\" \"big\" \"b\")
                                               \"test\")
                                       (\"VP\" (\"PP\" \"sea\" 1 2 (3 5 6)
                                                       4 (5 aa) 6))))
                            S
              /^^^^^^^^^^^^^^^^^^^^^^^^^\
              NP                        VP
           /^^^^^^^^^^^\                |
           NP         test              PP
       /^^^^^^|^^^\         /^^^|^^|^^^^|^^^|^^^|^^\
    Theeeee  big  b        sea  1  2    3   4   5  6
                                      /^^\      |
                                      5  6     AA
  "
  (labels ((to-strings (subtree)
             (let ((head (fmt " ~A " (atomize subtree))))
               (if (atom subtree) (list head)
                   (let* ((children (mapcar #'to-strings (cdr subtree)))
                          (merged (apply #'zip-with #'strcat
                                         (align-bottoms raw-below)))
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
         (center (floor (if (evenp width) (1+ width) width) 2))
         (spans (re:all-matches "[^\\s]+" string)))
    (if (= 2 (length spans))
        (strcat (filler center) "|" (filler (- width center)))
        (let ((pos (ceiling (+ (car spans) (cadr spans)) 2)))
          (fmt "~A/~{~A~}~A"
               (filler (1- pos))
               (loop :for (beg end . tail) :on (cddr spans) :by #'cddr
                  :for c := (ceiling (+ beg end) 2)
                  :collect (strcat (filler (- c pos 1) #\^)
                                   (if tail "|" "\\"))
                  :do (setf pos c) #+nil (print (list beg end c)))
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