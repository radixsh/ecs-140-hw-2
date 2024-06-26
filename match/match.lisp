(defun match (pattern assertion)
  (cond
    ((and (null pattern) (null assertion))
      t)
    ((or (null pattern) (null assertion))
      nil)
    ((equal (car pattern) (car assertion))
      (match (cdr pattern) (cdr assertion)))
    ((eql (car pattern) '?)
      (match (cdr pattern) (cdr assertion)))
    ((equal (car pattern) '!)
      (or (match (cdr pattern) (cdr assertion)) (match pattern (cdr assertion))))
    (t
      nil)
  )
)
