#|
                    ***** 8puzzle.lsp *****



Author: Alex Herman, Marcus HaberLing, Matthew Rames
Written Spring 2016 for CSC447/547 AI class.

|#

;--------------------------------------------------------------------------

(load 'solvable)
(load 'PuzzleFuncs)
(load 'DFID)
(load 'BFS)

(defun Start(args)
	(cond
		((> (length args) 1)
			(setf Puzzle (cdr args))
		)
		((= (length args) 1)
			(setf Puzzle (ReadInPuzzle (car args)))
		)
		((= (length args) 0)
			(format t "Enter a list of 9 numbers 0-8. 0 is the blank for the 8-puzzle: ")
			(setf Puzzle (read))
			(when (not (= (length Puzzle) 9 ))
				(Start args)
			)
		)
))

(defun ReadInPuzzle (filename)
	(with-open-file (in filename)
		(loop for num = (read in nil)
			until (null num)
				collect num
		)
	)
)

(Start *args*)
