(defun findtrue (listoflists)
    (cond
        ((null listoflists)
            nil
        )
        ((atom (car listoflists))
            (or (not (null (car listoflists))) (findtrue (cdr listoflists)))
        )
    )

)

(defun reachable (transition start final input)
    (cond
        ((null input)
            (eql start final)
        )
        (t
            (findtrue (mapcar
                (lambda (onestate)
                    (reachable transition onestate final (cdr input))
                )
                (funcall transition start (car input))
            ))
        )
    )
)
