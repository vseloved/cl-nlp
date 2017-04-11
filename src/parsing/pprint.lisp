;;; (c) 2013-2017 Vsevolod Dyomkin

(in-package #:nlp.parsing)
(named-readtables:in-readtable rutilsx:rutilsx-readtable)


(defun annotate-spans (tree &key (space 1) (idx 0))
  (let* ((str (str (first (mklist tree))))
         (len (length str))
         (max-height 0)
         (spans-heights (mapcar #`(multiple-value-list
                                   (annotate-spans % :space space :idx (1+ idx)))
                                (rest (mklist tree))))
         (child-spans (mapcar #'first spans-heights))
         (child-len (reduce '+ (mapcar #`(+ space (third %)) child-spans)
                            :initial-value (- space)))
         (has-children? (and (listp tree) child-spans)))
    (values (list (when (atom tree) idx)
                  str
                  (if has-children?
                      (if (>= child-len len)
                          child-len
                          (prog1 len
                            ;; add extra space to child spans
                            (mv-bind (whole rem) (round (- len child-len)
                                                        (length child-spans))
                              (loop :for s :in child-spans
                                 :while (or (plusp whole) (plusp rem)) :do
                                 (:- rem)
                                 (:= (third s)
                                     (pair (third s)
                                           (+ whole (if (minusp rem) 0 1))))))))
                      len)
                  child-spans
                  (if has-children?
                      (1+ (reduce 'max (mapcar 'fifth child-spans)))
                      0))
            (dolist (height (mapcar #'second spans-heights)
                     max-height)
              (:+ height)
              (when (> height max-height)
                (:= max-height height))))))

(defun spans->levels (spans target-height extra-pad
                      &key (space 1) (extra-space #h()) use-unicode-chars)
  (let* ((last (1- (length spans)))
         rez below)
    (push (with-output-to-string (out)
            (format out "~A" (filler (ceiling extra-pad 2)))
            (doindex (i span spans)
              (ds-bind (idx _ len &rest __) span
                (declare (ignore _ __))
                (let* ((pad (if (atom len) 0 (rt len)))
                       (len (+ (1- (atomize len)) pad))
                       (left (ceiling len 2))
                       (right (+ (floor len 2)
                                 (get# idx extra-space 0)))
                       (-fill-char (if use-unicode-chars #\─ #\-)))
                  (switch (i)
                    (0 (format out "~A~C~A"
                               (filler left)
                               (if (plusp last)
                                   (if use-unicode-chars #\┌ #\.)
                                   (if use-unicode-chars #\│ #\:))
                               (filler (+ right space)
                                       (if (plusp last) -fill-char #\Space))))
                    (last (format out "~A~C~A"
                                  (filler left -fill-char)
                                  (if use-unicode-chars #\┐ #\.)
                                  (filler (+ right space))))
                    (otherwise (format out "~A~C~A"
                                       (filler left -fill-char)
                                       (if use-unicode-chars #\┬ #\:)
                                       (filler (+ space right) -fill-char)))))))
            (format out "~A" (filler (floor extra-pad 2))))
          rez)
    (dolist (span spans)
      (ds-bind (idx str len child-spans height) span
        (let ((pad 0)
              cur)
          (unless (atom len)
            (:= len (lt len)
                pad (rt len)))
          (loop :for cur-height :from target-height :downto height :do
             (let* ((above? (> cur-height height))
                    (str (if above?
                             (if use-unicode-chars "│" ":")
                             str))
                    (extra (- (+ len pad) (length str)))
                    (left (ceiling extra 2))
                    (right (+ (- extra left)
                              (get# idx extra-space 0)))
                    (fmt (format nil "~A~A~A"
                                 (filler left) str (filler (+ right space)))))
               (if above?
                   (progn (push fmt cur)
                          (push fmt cur))
                   (push fmt cur))
               (unless above?
                 (push (append (reverse cur)
                               (when child-spans
                                 (spans->levels child-spans (1- cur-height)
                                                (+ extra-pad pad)
                                                :space space
                                                :use-unicode-chars
                                                use-unicode-chars)))
                       below)))))))
    (mapcar (lambda (&rest args)
              (push (fmt "~A~A~A"
                         (filler (ceiling extra-pad 2))
                         (apply 'reduce #`(fmt "~A~A" % %%)
                                args)
                         (filler (floor extra-pad 2)))
                    rez))
            (apply 'zip (reverse below)))
    (reverse rez)))

(defun tok-dist (sent left right space extra-space)
  (apply '+ -2
         (- (ceiling (- @left.end @right.beg)
                     2))
         (ceiling (- @left.end @right.beg)
                  2)
         (mapcar ^(+ space
                     (length (ss %))
                     (get# @%.id extra-space 0))
                 (loop :for id
                       :from @left.id
                       :below @right.id
                       :collect @sent.id))))

(defun format-rel (out line-char sent dep space extra-space)
  (with (((left right) (sort (list @dep.child @dep.head) '<
                             :key 'tok-id))
         (d (- (tok-dist sent left right space extra-space)
               (length (str @dep.rel))
               4)))
    (format out "`~A ~(~A~) ~A'"
            (filler (floor d 2) line-char)
            @dep.rel
            (filler (ceiling d 2) line-char))))

(defun format-connectors (out left-tok sent space extra-space
                          left-char mid-char right-char)
  (format out "~A~A"
          (if mid-char
              (fmt "~A ~A ~A" left-char mid-char right-char)
              right-char)
          (if (< @left-tok.id (1- (length sent)))
              (filler (- (tok-dist sent left-tok
                                   (? sent (1+ @left-tok.id))
                                   space extra-space)
                         4))
              "")))

(defun total-extra-space (sent extra-space begid endid)
  (mapcar ^(get# @%.id extra-space 0)
          (loop :for id :from begid :below endid
                :collect @sent.id)))

(defun deps->levels (sent deps
                     &key (space 1) (extra-space #h()) use-unicode-chars)
  (let ((left (make-array (length sent) :initial-element nil))
        (mid (make-array (length sent) :initial-element nil))
        (right (make-array (length sent) :initial-element nil))
        (con-char (if use-unicode-chars #\│ #\:))
        (line-char (if use-unicode-chars #\─ #\.))
        (levels (make-array (1- (length sent)) :initial-element nil))
        root
        depth)
    (dolist (dep deps)
      (when (eql 'dep:ROOT @dep.rel)
        (:= root dep)
        (return)))
    (labels ((rec (cur-dep deps)
               (let ((cur-id @cur-dep.child.token-id))
                 (dolist (dep (sort (remove-if-not ^(eql @cur-dep.child @%.head)
                                                   deps)
                                    '< :key #`(abs (- cur-id @%.child.token-id))))
                   (rec dep (remove dep deps))
                 (let* ((gid @dep.head.token-id)
                        (minid (min gid @dep.child.token-id))
                        (maxid (max gid @dep.child.token-id))
                        (mintok (? sent minid))
                        (maxtok (? sent maxid))
                        (d (tok-dist sent mintok maxtok space extra-space))
                        (min-d (+ 4 (length (str @dep.rel))
                                  ;; short head word needs additional space
                                  (max 0 (ceiling (- (if (= gid maxid) 4 5)
                                                     (length (str (? sent gid))))
                                                  2)))))
                   (when (< d min-d)
                     (:+ (get# (1- maxid) extra-space 0) (- min-d d)))))
                 (unless (eql 'dep:ROOT @cur-dep.rel)
                   (let* ((head-id @cur-dep.head.token-id)
                          (minid (min cur-id head-id))
                          (maxid (max cur-id head-id))
                          (level (reduce 'max
                                         (loop :for i :from minid :below maxid
                                            :collect (length (? levels i))))))
                     (loop :for i :from minid :below maxid :do
                        (loop :repeat (- level (length (? levels i))) :do
                           (push nil (? levels i)))
                        (push cur-dep (? levels i)))
                     (:= (? mid cur-id) level)
                     (if (= head-id maxid)
                         (:= (? left head-id) level)
                         (:= (? right head-id) level)))))))
      (rec root (remove root deps)))
    (:= depth (reduce 'max (loop :for i :from 0 :below (1- (length sent))
                              :collect (length (? levels i)))))
    (:= (? mid @root.child.token-id) depth)
    (dotimes (i (length levels))
      (reversef (? levels i)))
      (values
       (append (list
                ;; sent
                (with-output-to-string (out)
                  (doindex (i tok sent)
                    (format out "~A~A"
                            (str tok)
                            (filler (+ space (get# i extra-space 0))))))
                ;; arrows
                (with-output-to-string (out)
                  (write-string (filler (- (floor (length (str (? sent 0))) 2) 3))
                                out)
                  (dolist (tok sent)
                    (format-connectors
                     out tok sent space extra-space
                     (if (? left @tok.token-id) con-char #\Space)
                     (if use-unicode-chars #\↑ #\^)
                     (if (? right @tok.token-id) con-char #\Space)))))
               ;; rels
               (maptimes depth
                         (lambda (level)
                           (with-output-to-string (out)
                             (write-string (filler (floor (length (str (? sent 0)))
                                                          2))
                                           out)
                             (iter (:with i := 0)
                                   (:while (< i (length sent)))
                                   (if-it (and (< i (length levels))
                                               (? levels i level))
                                          (progn
                                            (format-rel out line-char sent it
                                                        space extra-space)
                                            (:= i (max @it.child.token-id
                                                       @it.head.token-id))
                                            (format out " ~A "
                                                    (if (and-it (? mid i)
                                                                (= it (1- level)))
                                                        con-char #\Space)))
                                          (progn
                                            (format-connectors
                                             out (? sent i) sent space extra-space
                                             (if (>= (or (? left i) -1) level)
                                                 con-char #\Space)
                                             (unless (and (plusp i)
                                                          (? levels (1- i) level))
                                               (if (>= (or (? mid i) -1) level)
                                                   con-char #\Space))
                                             (if (>= (or (? right i) -1) level)
                                                 con-char #\Space))
                                            (:+ i)))))))
               ;; root
               (list (fmt "~Aroot~%"
                          (filler (if (zerop @root.child.token-id)
                                      (- (floor (length (str @root.child)) 2) 3)
                                      (tok-dist sent (? sent 0) @root.child
                                                space extra-space))))))
       extra-space)))

;;;

(defun pprint-tree (tree
                    &key (space 1) (stream *standard-output*) use-unicode-chars)
  (mv-bind (span max-height) (annotate-spans tree :space space)
    (dolist (level (rest (spans->levels (list span) max-height 0 :space space
                                        :use-unicode-chars use-unicode-chars)))
      (write-line level stream))))

(defun pprint-tags (tree
                    &key (space 1) (stream *standard-output*) use-unicode-chars)
  (mv-bind (span max-height) (annotate-spans tree :space space)
    (dolist (level (last (spans->levels (list span) max-height 0 :space space
                                          :use-unicode-chars use-unicode-chars)
                         2))
      (write-line level stream))))

(defun pprint-deps (sent deps
                    &key (space 1) (stream *standard-output*) use-unicode-chars)
  (dolist (level (deps->levels sent deps :space space
                               :use-unicode-chars use-unicode-chars))
    (write-line level stream)))

(defun pprint-deps+tags (sent tree deps
                         &key (space 1) (stream *standard-output*) use-unicode-chars)
  (mv-bind (span max-height) (annotate-spans tree :space space)
    (mv-bind (deps-levels extra-space) (deps->levels sent deps :space space)
      ;; tags
      (dolist (level (last (spans->levels (list span) max-height 0 :space space
                                          :extra-space extra-space
                                          :use-unicode-chars use-unicode-chars)
                           2))
        (write-line level stream))
      ;; deps
      (dolist (level (rest deps-levels))
        (write-line level)))))

(defun pprint-syntax (sent tree deps
                      &key (space 1) (stream *standard-output*) use-unicode-chars)
  (mv-bind (span max-height) (annotate-spans tree :space space)
    (mv-bind (deps-levels extra-space) (deps->levels sent deps :space space)
      ;; consts
      (dolist (level (rest (spans->levels (list span) max-height 0 :space space
                                          :extra-space extra-space
                                          :use-unicode-chars use-unicode-chars)))
        (write-line level stream))
      ;; deps
      (dolist (level (rest deps-levels))
        (write-line level)))))


;;; helpers

(defun toks (words)
  (let ((offset 0)
        (id 0))
    (mapcar ^(with (((word &optional tag) (split #\_ %)))
               (prog1 (make-token :word word
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
            :head (if (= -1 (second %)) +ROOT+ (? sent (second %)))
            :child (? sent (third %)))
          deps))

(defpar *test-sent*
    (let ((sent (toks '("This" "is" "a" "simple" "test" "."))))
      (ds-bind (w-this w-is w-a w-simple w-test |w-.|) sent
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
                           (punct 1 5)))))))
