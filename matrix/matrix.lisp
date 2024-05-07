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


; ((3 4) (7 2) (5 9)) * ((3 1 5) (6 9 7))


; ((3 4) (2 1)) * ((1 5) (3 7))
; is
; [ 3 4 ]  [ 1 5 ]
; [ 2 1 ]  [ 3 7 ]

; [ 3 4 ]  [ 1 3 ]
; [ 2 1 ]  [ 5 7 ]

; First cell:   3*1 + 4*3 = 15
; Second cell:  3*5 + 4*7 = 43
; Third cell:   2*1 + 1*3 = 5
; Fourth cell:  2*5 + 1*7 = 17
; [ 15  43 ]
; [  5  17 ]

; Easier example:
; [1 2]  *  [3  4]
;           [4  7]
; = [1*3 + 2*4    1*4 + 2*7]

(defun dot-product (first second)
  (print 'dotting)
  (print first)
  (print second)
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
  (print 'get-row)
  (print first)
  (print second)
  (cond
    ((or (null first) (null second)) 
      nil)
    (t
      (print 'sendingtodot)
      (print (car first))
      (print (car second))
      (print 'innergettingrow)
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
      (print 'endingsoon)
      (list (get-row (car first) (matrix-transpose second)))
    )
    (t
      (list
        (get-row (car first) (matrix-transpose second))
        (get-row (car (cdr first)) (matrix-transpose second))
      )
    )
  )
)