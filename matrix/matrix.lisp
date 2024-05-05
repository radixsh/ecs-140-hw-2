(defun row-add (first second)
  (cond
    ((null first)
      nil
    )
    (t
      (cons (+ (car first) (car second)) 
        (row-add (cdr first) (cdr second))
      )
    )
  )
)

(defun matrix-add (first second)
  (cond
    ((null first) ; Second is nil iff first is null
      nil
    )
    ; matrix-add (1 2) (3 4)
    ((atom (car first)) ; Each matrix has only 1 row
      (row-add first second)
    )
    ((null (cdr first)) ; The next row after is empty
      (matrix-add (car first) (car second))
    )
    (t
      (list (matrix-add (car first) (car second))
            (matrix-add (cdr first) (cdr second))
      )
    )
  )
)

; ((1 2) (3 4)) --> (1 3)
(defun getfirst (listoflists)
  (cond
    ((null (car listoflists))
      nil
    )
    ((null (cdr listoflists)) ; look ahead
      (car (car listoflists))
    )
    (t
      ;(list 
        (cons (car (car listoflists)) (getfirst (cdr listoflists)))
        ;(list (cdr (car listoflists)) (cdr (car (cdr listoflists))))
      ;)
    )
  )
)

(defun getrem (listoflists)
  (cond
    ((null (car listoflists))
      nil
    )
    ((null (cdr listoflists))
      (cdr (car listoflists))
    )
    (t
      (cons (cdr (car listoflists)) (getrem (cdr listoflists)))
    )
  )
)

(defun matrix-transpose (mat)
  (cond
    ((null (car mat))
      nil
    )
    (t
      (cons
        (getfirst mat)
        (append (matrix-transpose (getrem mat)))
      )
    )
  )
)

(defun matrix-multiply (mat1 mat2)
  
)