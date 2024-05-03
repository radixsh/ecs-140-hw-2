(defun reachable (transition start final input)
    (cond
        ((null input)
            (eql start final)
        )
        (t
            (mapcar 
                (lambda (onestate)
                    (print onestate)
                    (reachable transition onestate final (cdr input))
                )
                (funcall transition start (car input))
            )
        )
    )
)
