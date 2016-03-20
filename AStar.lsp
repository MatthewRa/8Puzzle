(defun A* (puzz open closed)
  (cond
   ((correct (puz (top open)) (puz (top open))))
   
  )
)

(defun push (item list)
  (setf list (cons item list))
)

(defun pop (list)
  (let (x)
    (setf x (car list))
    (setf list (cdr list))
    x
  )
)

(defun top (list)
  (car (cdr list)) 
)

(defun correct (list)
  (equal list '(1 2 3 8 0 4 7 6 5 ))
  )

(defun puz (list)
  (cdr list)
)

(defun parent (list)
  (car list)
)
