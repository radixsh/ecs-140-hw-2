(defun pivot (n xs)
  (print 'lower)
  (print (lower n xs))
  (print 'upper)
  (print (upper n xs))
  (list (lower n xs) (upper n xs))
)

(defun lower (n mylist)
  (cond
    ((null mylist)
      nil
    )
    ((< (car mylist) n)
      (append (list (car mylist)) (lower n (cdr mylist)))
    )
    (t
      (lower n (cdr mylist))
    )
  )
)

(defun upper (n mylist)
  (cond
    ((null mylist)
      nil
    )
    ((>= (car mylist) n)
      (append (list (car mylist)) (upper n (cdr mylist)))
    )
    (t
      (upper n (cdr mylist))
    )
  )
)

(defun mylength (mylist)
  (if (null mylist) 
    0
    (+ 1 (mylength (cdr mylist)))
  )
)

(defun quicksort (xs)
  (print '(quicksort called on))
  (print xs)
  (cond 
    ((null xs)
      xs
    )
    ((equal (mylength xs) 1)
      xs)
    ((equal (mylength xs) 2)
      (print 'twoelements)
      (if (< (car xs) (car (cdr xs)))
        (list (car xs) (car (cdr xs)))
        (list (car (cdr xs)) (car xs))
      )
    )
    (t
      (print 'default)
      (let ((p (car xs)))
        (print 'callingpivot)
        (mapcar 'quicksort (pivot p xs))
      )
    )
  )
  
)