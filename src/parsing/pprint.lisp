;;; (c) 2013-2017 Vsevolod Dyomkin

(in-package #:nlp.parsing)
(named-readtables:in-readtable rutilsx:rutilsx-readtable)


(defun annotate-spans (tree &key (space 1) (id (vec -1)))
  (with ((str (ss (first (mklist tree))))
         (len (length str))
         (max-height 0)
         (spans-heights (mapcar ^(multiple-value-list
                                  (annotate-spans % :space space :id id))
                                (rest (mklist tree))))
         (child-spans (mapcar 'first spans-heights))
         (child-len (reduce '+ (mapcar ^(+ space (third %)) child-spans)
                            :initial-value (- space)))
         (has-children? (and (listp tree) child-spans)))
    (values (list (when (atom tree) (:+ (? id 0)))
                  str
                  (if has-children?
                      (if (>= child-len len)
                          child-len
                          (prog1 len
                            ;; add extra space to child spans
                            (with ((whole rem (round (- len child-len)
                                                     (length child-spans))))
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
            (dolist (height (mapcar 'second spans-heights)
                     max-height)
              (:+ height)
              (when (> height max-height)
                (:= max-height height))))))

(defun spans->levels (spans target-height extra-pad
                      &key (space 1) (extra-spaces #h()) utf-connectors)
  (let ((last (1- (length spans)))
        rez below)
    (push (with-output-to-string (out)
            (format out "~A" (filler (ceiling extra-pad 2)))
            (doindex (i span spans)
              (with (((id _ len &rest _) span)
                     (pad (if (atom len) 0 (rt len)))
                     (len (+ (1- (atomize len)) pad))
                     (left (ceiling len 2))
                     (right (+ (floor len 2)
                               (get# id extra-spaces 0)))
                     (-fill-char (if utf-connectors #\─ #\-)))
                  (switch (i)
                    (0 (format out "~A~C~A"
                               (filler left)
                               (if (plusp last)
                                   (if utf-connectors #\┌ #\.)
                                   (if utf-connectors #\│ #\:))
                               (filler (+ right space)
                                       (if (plusp last) -fill-char #\Space))))
                    (last (format out "~A~C~A"
                                  (filler left -fill-char)
                                  (if utf-connectors #\┐ #\.)
                                  (filler (+ right space))))
                    (otherwise (format out "~A~C~A"
                                       (filler left -fill-char)
                                       (if utf-connectors #\┬ #\:)
                                       (filler (+ space right) -fill-char))))))
            (format out "~A" (filler (floor extra-pad 2))))
          rez)
    (dolist (span spans)
      (with (((id str len child-spans height) span)
             (pad 0)
             (cur nil))
        (unless (atom len)
          (:= len (lt len)
              pad (rt len)))
        (loop :for cur-height :from target-height :downto height :do
          (with ((above? (> cur-height height))
                 (str (if above?
                          (if utf-connectors "│" ":")
                          str))
                 (extra (- (+ len pad) (length str)))
                 (left (ceiling extra 2))
                 (right (+ (- extra left)
                           (get# id extra-spaces 0)))
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
                                             ;; :extra-spaces extra-spaces
                                             :utf-connectors utf-connectors)))
                    below))))))
    (mapcar (lambda (&rest args)
              (push (fmt "~A~A~A"
                         (filler (ceiling extra-pad 2))
                         (apply 'reduce ^(fmt "~A~A" % %%)
                                args)
                         (filler (ceiling extra-pad 2)))
                    ;; (+ (get# (:+ id) extra-spaces 0))))
                    rez))
            (apply 'zip (reverse below)))
    (reverse rez)))

(defun tok-dist (toks left right space extra-spaces &optional lt rt)
  (apply '+
         (floor (- @right.end @right.beg)
                2)
         (1- (ceiling (- @left.end @left.beg)
                      2))
         (- (length (ss left)))
         (if lt -1 0) (if rt -1 0)
         (mapcar ^(+ (length (ss %))
                     space
                     (get# @%.id extra-spaces 0))
                 (loop :for id
                       :from @left.id
                       :below @right.id
                       :collect (? toks id)))))

(defun format-rel (out dep toks space extra-spaces lt mid rt
                   line-char conch level)
  (with (((left right) (sort (list @dep.tail @dep.head) '< :key 'tok-id))
         (d (- (tok-dist toks left right space extra-spaces t)
               (length (ss @dep.rel))
               2)))
    ;; (write-string (w/outstr (out) 
    (format out "~@[~A~]`~A ~(~A~) ~A´~@[~A~]~A"
            (strcat (when-it (? lt @left.id)
                      (fmt "~C" (if (> it (1- level)) conch #\Space)))
                    (unless (eql left @dep.tail)
                      (fmt "~C" (if (> (? mid @left.id) (1- level))
                                     conch #\Space))))
            (filler (floor d 2) line-char)
            @dep.rel
            (filler (ceiling d 2) line-char)
            (strcat (unless (eql right @dep.tail)
                      (fmt "~C" (if (> (? mid @right.id) (1- level))
                                     conch #\Space)))
                    (when-it (? rt @right.id)
                      (fmt "~C" (if (> it (1- level)) conch #\Space))))
            (filler (when (< @right.id (1- (length toks)))
                      (tok-dist toks right (? toks (1+ @right.id))
                                space extra-spaces
                                (? rt @right.id) (? lt (1+ @right.id))))))))
                  ;; out)))
                  


(defun format-connectors (out tok toks space extra-spaces lt mid rt level
                          mid-char conch)
  ;; (write-string (w/outstr (out)
  (format out "~@[~A~]~C~@[~A~]~@[~A~]"
          (when-it (? lt @tok.id)
            (fmt "~C" (if (> it (1- level)) conch #\Space)))
          (if (> (? mid @tok.id) (1- level))
              mid-char #\Space)
          (when-it (? rt @tok.id)
            (fmt "~C" (if (> it (1- level)) conch #\Space)))
          (when (< @tok.id (1- (length toks)))
            (filler (tok-dist toks tok (? toks (1+ @tok.id)) space extra-spaces
                              (? rt @tok.id) (? lt (1+ @tok.id)))))))
                ;; out))

(defun total-extra-space (toks extra-spaces begid endid)
  (mapcar ^(get# @%.id extra-spaces 0)
          (loop :for id :from begid :below endid
                :collect (? toks id))))

(defun deps->levels (deps &key (space 1) (extra-spaces #h()) utf-connectors)
  (with ((toks (sort (mapcar 'nlp:dep-tail deps) '< :key 'nlp:tok-id))
         (lt (make-array (length toks) :initial-element nil))
         (mid (make-array (length toks) :initial-element nil))
         (rt (make-array (length toks) :initial-element nil))
         (conch (if utf-connectors #\│ #\:))
         (levels (make-array (length toks) :initial-element nil))
         (root nil)
         (depth nil)
         (off 0))
    ;; pre-process toks
    (doindex (i tok toks)
      (unless @tok.beg
        (:= @tok.beg off
            @tok.end (:+ off (length @tok.word)))
        (:+ off)))
    ;; find root
    (dolist (dep deps)
      (when (eql 'dep:root @dep.rel)
        (:= root dep)
        (return)))
    ;; fill in levels array: for each token - the dependencies passing over it
    ;; in the process also record EXTRA-SPACES
    (labels ((rec (dep deps)
               (let ((id @dep.tail.id))
                 (dolist (d (sort (keep-if ^(eql @dep.tail @%.head)
                                           deps)
                                  '< :key ^(abs (- id @%.tail.id))))
                   (rec d (remove d deps))
                   (with ((hid @d.head.id)
                          (chid @d.tail.id)
                          (ltid (min hid chid))
                          (rtid (max hid chid))
                          (left (? toks ltid))
                          (right (? toks rtid))
                          (min-dist (+ 4 (length (ss @d.rel)))))
                     (if (= hid rtid)
                         (:= (get# (1- hid) extra-spaces)
                             (max (get# (1- hid) extra-spaces 0)
                                  (floor (- 3 (length (ss (? toks hid))))
                                         2)))
                         (:= (get# hid extra-spaces)
                             (max (get# hid extra-spaces 0)
                                  (ceiling (- 3 (length (ss (? toks hid))))
                                           2))))
                     (:+ (get# ltid extra-spaces 0)
                         (max 0 (- min-dist (tok-dist toks left right
                                                      space extra-spaces t))))))
                 (unless (eql 'dep:ROOT @dep.rel)
                   (with ((hid @dep.head.id)
                          (ltid (min id hid))
                          (rtid (max id hid))
                          (level (reduce 'max
                                         (loop :for i :from ltid :to rtid
                                               :collect (length (? levels i))))))
                     (loop :for i :from ltid :to rtid :do
                       (loop :repeat (- level (length (? levels i))) :do
                         (push nil (? levels i)))
                       (push dep (? levels i)))
                     (:= (? mid id) level)
                     (if (= hid rtid)
                         (:= (? lt hid) level)
                         (:= (? rt hid) level)))))))
      (rec root (remove root deps)))
    ;; add root dep and arrange levels properly
    (:= depth (reduce 'max (loop :for i :from 0 :below (1- (length toks))
                                 :collect (length (? levels i)))))
    (:= (? mid @root.tail.id) depth)
    (loop :repeat (- depth (length (? levels @root.tail.id))) :do
      (push root (? levels @root.tail.id)))
    (dotimes (i (length levels))
      (:= (? levels i) (reverse (? levels i))))
    ;; output resuting level strings
    (values (append (list
                     ;; toks
                     (with-output-to-string (out)
                       (doindex (i tok toks)
                         (format out "~A~A" (ss tok)
                                 (filler (+ space (get# i extra-spaces 0))))))
                     ;; arrows
                     (with-output-to-string (out)
                       (write-string (filler (floor (length (ss (? toks 0)))
                                                    2))
                                     out)
                       (dolist (tok toks)
                         (format-connectors
                          out tok toks space extra-spaces lt mid rt -1
                          (if utf-connectors #\↑ #\^)
                          (if utf-connectors #\│ #\.)))))
                    ;; rels
                    (maptimes
                     depth
                     (lambda (level)
                       (with-output-to-string (out)
                         (write-string (filler (floor (length (ss (? toks 0)))
                                                      2))
                                       out)
                         (iter (:with i := 0)
                               (:while (< i (length toks)))
                               (if-it (? levels i level)
                                      (progn
                                        (format-rel out it toks
                                                    space extra-spaces lt mid rt
                                                    (if utf-connectors #\─ #\.)
                                                    conch level)
                                        (:= i (max @it.tail.id
                                                   @it.head.id)))
                                      (format-connectors
                                       out (? toks i) toks space extra-spaces
                                       lt mid rt level conch conch))
                               (:+ i)))))
                    ;; root
                    (list (fmt "~Aroot~%"
                               (filler
                                (+ (1- (floor (length (ss (? toks 0)))
                                              2))
                                   (if (plusp @root.tail.id)
                                       (tok-dist toks (? toks 0) @root.tail
                                                 space extra-spaces)
                                       0))))))
            extra-spaces)))


;;; pprint

(defun pprint-tree (tree &key (space 1) (stream *standard-output*)
                           utf-connectors)
  (with ((span max-height (annotate-spans tree :space space)))
    (dolist (level (rest (spans->levels (list span) max-height 0 :space space
                                        :utf-connectors utf-connectors)))
      (write-line level stream))))

(defun pprint-tags (tree &key (space 1) (stream *standard-output*)
                           utf-connectors)
  (with ((span max-height (annotate-spans tree :space space)))
    (doindex (i level (last (spans->levels (list span) max-height 0 :space space
                                           :utf-connectors utf-connectors)
                            3))
      (unless (= 1 i)
        (write-line level stream)))))

(defun pprint-deps (deps &key (space 1) (stream *standard-output*)
                           utf-connectors)
  (dolist (level (deps->levels deps :space space
                               :utf-connectors utf-connectors))
    (write-line level stream)))

(defun pprint-deps+tags (tree deps &key (space 1) (stream *standard-output*)
                                     utf-connectors)
  (with ((span max-height (annotate-spans tree :space space))
         (deps-levels extra-spaces (deps->levels deps :space space)))
    ;; tags
    (doindex (i level (last (spans->levels (list span) max-height 0 :space space
                                           :extra-spaces extra-spaces
                                           :utf-connectors utf-connectors)
                            3))
      (unless (= 1 i)
        (write-line level stream)))
    ;; deps
    (dolist (level (rest deps-levels))
      (write-line level))))

(defun pprint-syntax (tree deps &key (space 1) (stream *standard-output*)
                                  utf-connectors)
  (with ((span max-height (annotate-spans tree :space space))
         (deps-levels extra-spaces (deps->levels deps :space space)))
      ;; consts
      (dolist (level (rest (spans->levels (list span) max-height 0 :space space
                                          :extra-spaces extra-spaces
                                          :utf-connectors utf-connectors)))
        (write-line level stream))
      ;; deps
      (dolist (level (rest deps-levels))
        (write-line level))))
