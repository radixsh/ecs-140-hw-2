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
(defun get-firsts (listoflists)
  (cond
    ((null (car listoflists))
      nil
    )
    ((null (cdr listoflists)) ; look ahead
      (cons (car (car listoflists)) nil)
    )
    (t
      (cons (car (car listoflists)) (get-firsts (cdr listoflists)))
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
        (get-firsts mat)
        (matrix-transpose (get-rest mat))
      )
    )
  )
)

(defun dot-product (first second)
  (cond
    ((null (cdr first)) ; (cdr second) will also be nil
      (* (car first) (car second))
    )
    (t
      (+ 
        (* (car first) (car second))
        (dot-product (cdr first) (cdr second))
      )
    )
  )
)

; Receives second matrix already transposed
(defun get-row (first second)
  (cond
    ((or (null first) (null second)) 
      nil)
    (t
      (cons
        (dot-product first (car second))
        (get-row first (cdr second))
      )
    )
  )
)

(defun matrix-multiply (first second)
  (cond
    ((null first)
      nil
    )
    ((null (cdr first))
      (list (get-row (car first) (matrix-transpose second)))
    )
    (t
      (cons
        (get-row (car first) (matrix-transpose second))
        (matrix-multiply (cdr first) second)
      )
    )
  )
)