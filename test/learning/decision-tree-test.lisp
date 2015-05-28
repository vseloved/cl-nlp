;;; (c) 2015 Vsevolod Dyomkin

(in-package #:nlearn)
(named-readtables:in-readtable rutils-readtable)


(defparameter *epsilon* 0.01)

(defun ~= (x y)
  (< (abs (- x y)) *epsilon*))


(defparameter *golf-data*
  (let (labels data)
    (iter (:for line :in (split #\Newline "
OUTLOOK | TEMPERATURE | HUMIDITY | WINDY | PLAY
=====================================================
sunny   |      85     |    85    | false | Don't Play
sunny   |      80     |    90    | true  | Don't Play
overcast|      83     |    78    | false | Play
rain    |      70     |    96    | false | Play
rain    |      68     |    80    | false | Play
rain    |      65     |    70    | true  | Don't Play
overcast|      64     |    65    | true  | Play
sunny   |      72     |    95    | false | Don't Play
sunny   |      69     |    70    | false | Play
rain    |      75     |    80    | false | Play
sunny   |      75     |    70    | true  | Play
overcast|      72     |    90    | true  | Play
overcast|      81     |    75    | false | Play
rain    |      71     |    80    | true  | Don't Play
" :remove-empty-subseqs t))
          (if (:first-time-p)
              (:= labels (map-into (make-array 5)
                                   #`(mkeyw (string-trim " " %))
                                   (split #\| line)))
              (when (alpha-char-p (char line 0))
                (let ((cur (mapcar #`(let ((item (string-trim " " %)))
                                       (cond ((member item '("true" "Play")
                                                      :test 'string=)
                                              t)
                                             ((member item '("false" "Don't Play")
                                                      :test 'string=)
                                              nil)
                                             ((digit-char-p (char item 0))
                                              (parse-integer item))
                                             (t (mkeyw item))))
                                   (split #\| line))))
                  (push (pair (coerce (butlast cur) 'vector) (last1 cur))
                        data)))))
    (cons (reverse data) labels)))

(deftest entropy ()
  (should be = 1.0 (entropy '(0.5 0.5)))
  (should be ~= 0.914 (entropy '(0.67 0.33)))
  (should be = 0 (entropy '(1 0 0 0)))
  (should be ~= 0.693 (entropy (car *golf-data*)
                               :idx (position :outlook (cdr *golf-data*))
                               :key 'rt)))

(deftest info-gain ()
  (should be ~= 0.246
          (info-gain (car *golf-data*)
                     :idx (position :outlook (cdr *golf-data*)))))

(deftest split-info ()
  (should be ~= 1.577
          (split-info (car *golf-data*) (position :outlook (cdr *golf-data*)))))

(deftest gini-idx ()
  (should be zerop (gini-idx '((#(1 2) t))))
  (should be = 1/2 (gini-idx '((#(1 2) t)
                               (#(1 2) t)
                               (#(1 2) nil)
                               (#(1 2) nil)))))

(deftest gini-split-idx ()
  (should be zerop (gini-split-idx '(((#(1 2) t)
                                      (#(1 2) t))
                                     ((#(1 2) nil)
                                      (#(1 2) nil)))))
  (should be = 2/5 (gini-split-idx '(((#(1 2) t))
                                     ((#(1 2) t)
                                      (#(1 2) t)
                                      (#(1 2) nil)
                                      (#(1 2) nil))))))

(deftest c4.5-train ()
  (should be equal
          '(case (elt % 0)
            (:sunny
             (if (<= (elt % 2) 70)
                 t
                 nil))
            (:overcast t)
            (:rain (case (elt % 3) (t nil) (() t))))
          (train (make 'c4.5-tree) (car *golf-data*))))

(deftest cart-train ()
  (should be equal
          '(if (eql :sunny (elt % 0))
            (:sunny
             (if (<= (elt % 2) 70)
                 t
                 nil))
            (:overcast t)
            (:rain (case (elt % 3) (t nil) (() t))))
          (train (make 'cart-tree) (car *golf-data*))))
