(defun reachable (transition start final input)
    (cond
        ((null input)
            (eql start final)
        )
        (t
            (let (listtoflatten 
                (carmap
                    (lambda (onestate)
                        (reachable transition onestate final (cdr input))
                    )
                    (funcall transition start (car input))
                ))

                (funcall
                    (lambda (thing)
                        (print thing)
                    ) 
                    listtoflatten
                )
            )
            
        )
    )
)
