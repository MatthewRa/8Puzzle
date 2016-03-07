#|
                    ***** 8puzzle.lsp *****



Author: Alex Herman, Marcus HaberLing, Matthew Rames
Written Spring 2016 for CSC447/547 AI class.

|#

;--------------------------------------------------------------------------

(load 'solvable)

(defun ReadInPuzzle (filename)
	(with-open-file (in filename)
		(loop for num = (read in nil)
			until (null num)
				collect num
		)
	)
)

(defun IsSolved (list)
  (equal list '(1 2 3 8 0 4 7 6 5))
)

(setf Puzzle (ReadInPuzzle (car *args*)))

(print Puzzle)
