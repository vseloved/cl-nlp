;;; (c) 2015-2017 Vsevolod Dyomkin

(in-package #:nlearn)
(named-readtables:in-readtable rutilsx-readtable)


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
                                              (float (parse-integer item)))
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
  (should be = 0.4 (gini-split-idx '(((#(1 2) t))
                                     ((#(1 2) t)
                                      (#(1 2) t)
                                      (#(1 2) nil)
                                      (#(1 2) nil))))))

(deftest c4.5-train ()
  (should be equal
          '(case (? % 0)
            (:rain (case (? % 3)
                     (nil (pair t 1.0))
                     (t (pair nil 1.0))))
            (:overcast (pair t 1.0))
            (:sunny
             (if (%= <= 2 70.0)
                 (pair t 1.0)
                 (pair nil 1.0))))
          (tree-repr (train (make 'c4.5-tree) (car *golf-data*)))))

(deftest cart-train ()
  (should be equal
          '(if (%= eql 0 :overcast)
               (pair t 1.0)
               (if (%= <= 1 75.0)
                   (if (%= <= 1 65.0)
                       (pair nil 1.0)
                       (if (%= <= 1 70.0)
                           (pair t 1.0)
                           (if (%= <= 1 72.0)
                               (pair nil 1.0)
                               (pair t 1.0))))
                   (pair nil 1)))
          (tree-repr (train (make 'cart-tree) (car *golf-data*)))))
