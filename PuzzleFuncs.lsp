#|
                    ***** PuzzleFuncs.lsp *****



Author: Alex Herman, Marcus HaberLing, Matthew Rames
Written Spring 2016 for CSC447/547 AI class.

Description: This file contains the functions that are used to move the blank
	around in the puzzle. It also contains a function to find if a puzle is solved.

|#

;--------------------------------------------------------------------------

(defvar *EXPANDED* 0)
(defvar *GENERATED* 0)
(defvar *DISTINCT* 0)

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
