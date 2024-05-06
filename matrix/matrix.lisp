(defun row-add (first second)
  (cond
    ((null first)
      nil
    )
    (t
      (cons
        (+ (car first) (car second)) 
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
    ((atom (car first)) ; Each matrix has only 1 row
      (row-add first second)
    )
    ((null (cdr first)) ; The next row after is empty
      (cons (matrix-add (car first) (car second)) nil)
    )
    (t
      (cons (matrix-add (car first) (car second))
            (matrix-add (cdr first) (cdr second))
      )
    )
  )
)

; ((1 2) (3 4)) --> (1 3)
(defun get-first (listoflists)
  (cond
    ((null (car listoflists))
      nil
    )
    ((null (cdr listoflists)) ; look ahead
      (cons (car (car listoflists)) nil)
    )
    (t
      (cons (car (car listoflists)) (get-first (cdr listoflists)))
    )
  )
)

(defun get-rest (listoflists)
  (cond
    ((null (car listoflists))
      nil
    )
    ((null (cdr listoflists))
      (cons (cdr (car listoflists)) nil)
    )
    (t
      (cons (cdr (car listoflists)) (get-rest (cdr listoflists)))
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
        (get-first mat)
        (matrix-transpose (get-rest mat))
      )
    )
  )
)

(defun matrix-multiply (mat1 mat2)
  
)