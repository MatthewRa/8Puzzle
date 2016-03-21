#|
                    ***** PuzzleFuncs.lsp *****



Author: Alex Herman, Marcus HaberLing, Matthew Rames
Written Spring 2016 for CSC447/547 AI class.

|#

;--------------------------------------------------------------------------

(defun IsSolved (list)
	(equal list '(1 2 3 8 0 4 7 6 5))
)

(defun MoveBlankUp (list)
	(let ((x (position 0 list)) (nlist (copy-list list)))
		(when (> x 2)
				(setf (nth x nlist) (nth (- x 3) nlist) )
				(setf (nth (- x 3) nlist) 0 )
				nlist
		)
	)
)

(defun MoveBlankDown (list)
		(let ((x (position 0 list)) (nlist (copy-list list)))
		(when (< x 6)
				(setf (nth x nlist) (nth (+ x 3) nlist) )
				(setf (nth (+ x 3) nlist) 0 )
				nlist
		)
	)
)

(defun MoveBlankRight (list)
	(let ((x (position 0 list)) (nlist (copy-list list)))
		(cond
			( (= (mod (+ x 1) 3) 0) nil)
			(t
				(setf (nth x nlist) (nth (+ x 1) nlist) )
				(setf (nth (+ x 1) nlist) 0 )
				nlist
			)
		)
	)
)

(defun MoveBlankLeft (list)
	(let ((x (position 0 list)) (nlist (copy-list list)))
		(cond
			( (= (mod (+ x 1) 3) 1) nil)
			(t
				(setf (nth x nlist) (nth (- x 1) nlist) )
				(setf (nth (- x 1) nlist) 0 )
				nlist
			)
		)
	)
)
