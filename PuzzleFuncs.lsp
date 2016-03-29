#|
                    ***** PuzzleFuncs.lsp *****



Author: Alex Herman, Marcus HaberLing, Matthew Rames
Written Spring 2016 for CSC447/547 AI class.

Description: This file contains the functions that are used to move the blank
	around in the puzzle. It also contains a function to find if a puzle is solved.

|#

;--------------------------------------------------------------------------

(defun IsSolved (list)
	"This function returns T if the puzzle is solved and NIL if not."
	(equal list '(1 2 3 8 0 4 7 6 5))
)

(defun MoveBlankUp (list)
	"This function returns the list with the blank moved up, or NIL if moving the
	blank in this direction is not possible."
	(let ((x (position 0 list)) (nlist (copy-list list)))
		(when (> x 2)
				(setf (nth x nlist) (nth (- x 3) nlist) )
				(setf (nth (- x 3) nlist) 0 )
				nlist
		)
	)
)

(defun MoveBlankDown (list)
	"This function returns the list with the blank moved down, or NIL if moving the
	blank in this direction is not possible."
	(let ((x (position 0 list)) (nlist (copy-list list)))
		(when (< x 6)
				(setf (nth x nlist) (nth (+ x 3) nlist) )
				(setf (nth (+ x 3) nlist) 0 )
				nlist
		)
	)
)

(defun MoveBlankRight (list)
	"This function returns the list with the blank moved right, or NIL if moving the
	blank in this direction is not possible."
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
	"This function returns the list with the blank moved left, or NIL if moving the
	blank in this direction is not possible."
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
	"This function prints each stage of the solution in solving an 8 puzzle."
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
