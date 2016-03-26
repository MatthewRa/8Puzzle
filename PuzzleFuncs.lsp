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

(defun printSolution (lst)
		(cond
				((null lst) nil)
				(t
            (do ((x 0 (+ x 4))) ((>= x (length lst)) nil)
                (do ((y 0 (1+ y))) ((> y 3) nil)
									(when (nth (+ x y) lst)
	                  (if (equal (nth 0 (nth (+ x y) lst)) 0) (princ " ") (princ (nth 0 (nth (+ x y) lst))))
	                  (princ " ")
	                  (if (equal (nth 1 (nth (+ x y) lst)) 0) (princ " ") (princ (nth 1 (nth (+ x y) lst))))
	                  (princ " ")
	                  (if (equal (nth 2 (nth (+ x y) lst)) 0) (princ " ") (princ (nth 2 (nth (+ x y) lst))))
	                  (princ "      ")
									)
                )
                (princ #\return)
                (princ #\linefeed)
                (do ((y 0 (1+ y))) ((> y 3) nil)
									(when (nth (+ x y) lst)
	                  (if (equal (nth 3 (nth (+ x y) lst)) 0) (princ " ") (princ (nth 3 (nth (+ x y) lst))))
	                  (princ " ")
	                  (if (equal (nth 4 (nth (+ x y) lst)) 0) (princ " ") (princ (nth 4 (nth (+ x y) lst))))
	                  (princ " ")
	                  (if (equal (nth 5 (nth (+ x y) lst)) 0) (princ " ") (princ (nth 5 (nth (+ x y) lst))))
	                  (if (null (IsSolved (nth (+ x y) lst))) (princ "  ->  "))
									)
                )
                (princ #\return)
                (princ #\linefeed)
                (do ((y 0 (1+ y))) ((> y 3) nil)
									(when (nth (+ x y) lst)
	                  (if (equal (nth 6 (nth (+ x y) lst)) 0) (princ " ") (princ (nth 6 (nth (+ x y) lst))))
	                  (princ " ")
	                  (if (equal (nth 7 (nth (+ x y) lst)) 0) (princ " ") (princ (nth 7 (nth (+ x y) lst))))
	                  (princ " ")
	                	(if (equal (nth 8 (nth (+ x y) lst)) 0) (princ " ") (princ (nth 8 (nth (+ x y) lst))))
	                  (princ "      ")
									)
                )
                (princ #\return)
                (princ #\linefeed)
                (princ #\return)
                (princ #\linefeed)

            )
				)
		)
)
