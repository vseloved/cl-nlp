(defun plot-cfd (cfd &rest args
                 &key conditions samples cumulative (order-by (constantly nil)))
  "Plot all or selected CONDITIONS and SAMPLES from CFD.
   CUMULATIVE counts may be used, as well as custom ordering with ORDER-BY"
  (cgn:with-gnuplot (t)
    (cgn:format-gnuplot "set grid")
    (cgn:format-gnuplot "set xtics rotate 90 1789,5,2009")
    (cgn:format-gnuplot "set ylabel \"~@[Cumulative ~]Counts\"" cumulative)
    (let ((format-string (fmt "\"~A\" using 1:~~A with lines ls ~~:*~~A title ~
                               columnheader(~~:*~~A)"
                              (apply #'ncore::cfd->tsv cfd args))))
      (cgn:format-gnuplot
       "plot [~A:~A] ~A" 1789 2009
       (strjoin "," (mapcar #`(fmt format-string (+ 2 %))
                            (range 0 (length conditions))))))))