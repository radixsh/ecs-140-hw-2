(defun pivot (n xs)
  (list (lower n xs) (list n) (upper n xs))
)

(defun lower (n mylist)
  (cond
    ((or (null mylist))
      nil
    )
    ((null (car mylist))
      (lower n (cdr mylist))
    )
    ((< (car mylist) n)
      (cons (car mylist) (lower n (cdr mylist)))
    )
    (t
      (lower n (cdr mylist))
    )
  )
)

(defun upper (n mylist)
  (cond
    ((or (null mylist) (null (car mylist)))
      nil
    )
    ((null (car mylist))
      (upper n (cdr mylist))
    )
    ((>= (car mylist) n)
      (cons (car mylist) (upper n (cdr mylist)))
    )
    (t
      (upper n (cdr mylist))
    )
  )
)

(defun quicksort (xs)
  (print '(quicksort called on))
  (print xs)
  (cond
    ((null xs) 
      nil
    )
    ((null (lower (car xs) xs))
      (cons (car xs) (quicksort (cdr xs)))
    )
    (t
      ;( 'quicksort (pivot (car xs) xs))
      (append (quicksort (lower (car xs) xs)) 
              (quicksort (upper (car xs) xs)))
    )
  )
)