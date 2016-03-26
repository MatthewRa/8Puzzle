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
			(setf Puzzle args)
		)
		((= (length args) 1)
			(setf Puzzle (ReadInPuzzle (car args)))
		)
		((= (length args) 0)
			(format t "Enter a list of 9 numbers 0-8 for the 8 puzzle. ex. (1 2 3 4 0 5 6 7 8): ")
			(setf Puzzle (read))
			(when (not (= (length Puzzle) 9 ))
				(format t "Error: Invalid list~%")
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

; main function
(defun main ( args )
    "(main args): emulate a main function, called with command-line args"
    (format t "~D command line arguments: ~A~%" (length args) args)
	(Start args)
	(format t "Starting Puzzle: ~A~%~%" Puzzle)

	(when (null (solvable Puzzle))
		(format t "Puzzle not solvable...~%")
		(format t "Exiting...~%")
		(exit)
	)

	(format t "BFS graph search ~%")
	(format t "---------------- ~%")
	(setf SolutionPath (bfs Puzzle))
	(format t "Solution found in ~d moves~%" (length SolutionPath))
	(format t "~d nodes generated (~d distinct nodes), ~d nodes expanded~%~%" *GENERATED* *DISTINCT* *EXPANDED*)

	(format t "Soltuion Path: ~A~%~%" SolutionPath)

	(format t "DFID graph search ~%")
	(format t "----------------- ~%")
    (setf SolutionPath (DFID Puzzle))
	(format t "Solution found in ~d moves~%" (length SolutionPath))
    (format t "~d nodes generated (~d distinct nodes), ~d nodes expanded~%~%" *GENERATED* *DISTINCT* *EXPANDED*)

    (format t "Soltuion Path: ~A~%~%" SolutionPath)

	(format t "A* graph search ~%")
	(format t "--------------- ~%")
)

; call the main function, passing command-line arguments
(main *ARGS*)
