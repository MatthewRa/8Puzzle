#|
                    ***** 8puzzle.lsp *****



Author: Alex Herman, Marcus HaberLing, Matthew Rames
Written Spring 2016 for CSC447/547 AI class.

|#

;--------------------------------------------------------------------------

(load 'solvable)
(load 'PuzzleFuncs)

(defun Start(args)
	(cond
			
		((= (length args) 1)	
			(setf Puzzle (ReadInPuzzle (car args)))
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

(print Puzzle)

