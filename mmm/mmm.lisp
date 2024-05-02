(defun mylength (mylist)
  (if (null mylist) 
    0
    (+ 1 (mylength (cdr mylist)))
  )
)

(defun mymax (mylist)
  (cond
    ((null mylist)
      -99999999
    )
    ((equal (mylength mylist) 2)
      (if (> (car mylist) (car (cdr mylist)))
        (car mylist)
        (car (cdr mylist))
      )
    )
    ((> (car mylist) (mymax (cdr mylist)))
      (car mylist)
    )
    (T
      (mymax (cdr mylist))
    )
  )
)

(defun mymin (mylist)
  (cond
    ((null mylist)
      9999999999
    )
    ((equal (mylength mylist) 2)
      (if (< (car mylist) (car (cdr mylist)))
        (car mylist)
        (car (cdr mylist))
      )
    )
    ((< (car mylist) (mymin (cdr mylist)))
      (car mylist)
    )
    (T
      (mymin (cdr mylist))
    )
  )
)

(defun mysum (mylist)
  (cond
    ((null mylist) 0)
    (T (+ (car mylist) (mysum (cdr mylist))))
  )
)

(defun myavg (mylist)
  (/ (mysum mylist) (mylength mylist))
)

(defun min-mean-max (xs)
  (list (mymin xs) (myavg xs) (mymax xs))
)
