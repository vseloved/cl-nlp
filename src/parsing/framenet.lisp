(dotable (frame raw *frames*)
  (let ((roles #h(equal)))
    (re:do-matches (beg end "Roleset id: <font color=green>[^<]+</font>" raw)
      (let ((b (1+ (position #\> raw :start beg)))
            (e (search "<h4>" raw :start2 (1+ (search "<h4>" raw
                                                      :start2 beg :test #'string=))
                                  :test #'string=))
            args)
        (re:do-register-groups (arg)
            ("&nbsp&nbsp&nbsp&nbsp&nbsp&nbsp&nbsp <b>([^<]+)"
             raw nil :start b :end e)
          (push arg args))
        (set# (print (slice raw b (position #\< raw :start (1+ b))))
              roles (print (reverse args)))))))
