#|
                    ***** 8puzzle.lsp *****



Author: Alex Herman, Marcus HaberLing, Matthew Rames
Written Spring 2016 for CSC447/547 AI class.

|#

;--------------------------------------------------------------------------

(load 'solvable)
(load 'PuzzleFuncs)
(load 'AStar)
(load 'DFID)
(load 'BFS)

(defun Start(args)
	"This function gets the starting puzzle from the file or the command line and
	then returns to the main function."
	(cond
		((> (length args) 1)
			(setf Puzzle args)
			;(setf Puzzle (mapcar #'parse-integer Puzzle))
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
	"This function reads a puzzle from the file specified in the command line arguments."
	(with-open-file (in filename)
		(loop for num = (read in nil)
			until (null num)
				collect num
		)
	)
)



(defun 8puzzle ( &optional args )
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
	(format t "Solution found in ~d moves~%" (- (length SolutionPath) 1))
	(format t "~d nodes generated (~d distinct nodes), ~d nodes expanded~%~%" *GENERATED* *DISTINCT* *EXPANDED*)
	(printSolution SolutionPath)

	(format t "DFID graph search ~%")
	(format t "----------------- ~%")
  (setf SolutionPath (DFID Puzzle))
	(format t "Solution found in ~d moves~%" (- (length SolutionPath) 1))
  (format t "~d nodes generated (~d distinct nodes), ~d nodes expanded~%~%" *GENERATED* *DISTINCT* *EXPANDED*)
	(printSolution SolutionPath)

	(format t "A* graph search (Heuristic: tiles out of place) ~%")
	(format t "--------------- ~%")
	(setf SolutionPath (A* Puzzle #'h-misplaced-tiles))
	(format t "Solution found in ~d moves~%" (- (length SolutionPath) 1))
	(format t "~d nodes generated (~d distinct nodes), ~d nodes expanded~%~%" *GENERATED* *DISTINCT* *EXPANDED*)
	(printSolution SolutionPath)

	(format t "A* graph search (Heuristic: tiles out of place) ~%")
	(format t "--------------- ~%")
	(setf SolutionPath (A* Puzzle #'h-manhattan-distance))
	(format t "Solution found in ~d moves~%" (- (length SolutionPath) 1))
	(format t "~d nodes generated (~d distinct nodes), ~d nodes expanded~%~%" *GENERATED* *DISTINCT* *EXPANDED*)
	(printSolution SolutionPath)

	(format t "A* graph search (Heuristic: tiles out of place) ~%")
	(format t "--------------- ~%")
	(setf SolutionPath (A* Puzzle #'h-manhattan-squared))
	(format t "Solution found in ~d moves~%" (- (length SolutionPath) 1))
	(format t "~d nodes generated (~d distinct nodes), ~d nodes expanded~%~%" *GENERATED* *DISTINCT* *EXPANDED*)
	(printSolution SolutionPath)
)

(when (= (length *ARGS*) 1)
	(8puzzle *ARGS*)
)
