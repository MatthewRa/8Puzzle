#|
                    ***** 8puzzle.lsp *****



Author: Alex Herman, Marcus HaberLing, Matthew Rames
Written Spring 2016 for CSC447/547 AI class.

	8puzzle is the starting point of the application. If an argument is passed in
	through command line (ex. clisp 8puzzle.lsp easy.puz) then the puzzle file is
	read and stored into a list. If 8puzzle is started through clisp (ex. (load '8puzzle))
	then you can type (8puzzle '[optionalList]) you can either give 8puzzle a list or
	it will prompt you to enter a list for the puzzle. Then different search algorithms are ran on the
	puzzle to find the solution. BFS, DFID, and three different A* searches, tiles out of place,
	Manhattan Distance, and Manhattan Distance Squared. The solution paths for each search is
	then outputed to the screen.

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
	"This is the main logic of the program. It prints the solution to an 8puzzle
	found using 5 different search strategies."
	(Start args)
	(format t "Starting Puzzle: ~A~%~%" Puzzle)

	; Make sure the starting puzzle is in a valid position
	(when (null (solvable Puzzle))
		(format t "Puzzle not solvable...~%")
		(format t "Exiting...~%")
		(exit)
	)

	; Use breadth first search
	(format t "BFS graph search ~%")
	(format t "---------------- ~%")
	(setf SolutionPath (bfs Puzzle))
	(format t "Solution found in ~d moves~%" (- (length SolutionPath) 1))
	(format t "~d nodes generated (~d distinct nodes), ~d nodes expanded~%~%" *GENERATED* *DISTINCT* *EXPANDED*)
	(printSolution SolutionPath)
	(setf *GENERATED* 0)
	(setf *DISTINCT* 0)
	(setf *EXPANDED* 0)

	; Use Depth First Search with Iterated Deepening
	(format t "DFID graph search ~%")
	(format t "----------------- ~%")
  (setf SolutionPath (DFID Puzzle))
	(format t "Solution found in ~d moves~%" (- (length SolutionPath) 1))
  (format t "~d nodes generated (~d distinct nodes), ~d nodes expanded~%~%" *GENERATED* *DISTINCT* *EXPANDED*)
	(printSolution SolutionPath)

	; Use an A* search with the admissable heuristic number of tiles out of place
	(format t "A* graph search (Heuristic: tiles out of place) ~%")
	(format t "--------------- ~%")
	(setf SolutionPath (A* Puzzle #'h-misplaced-tiles))
	(format t "Solution found in ~d moves~%" (- (length SolutionPath) 1))
	(format t "~d nodes generated (~d distinct nodes), ~d nodes expanded~%~%" *GENERATED* *DISTINCT* *EXPANDED*)
	(printSolution SolutionPath)

	; Use an A* search with the admissable heuristic manhattan distance
	(format t "A* graph search (Heuristic: Manhattan Distance) ~%")
	(format t "--------------- ~%")
	(setf SolutionPath (A* Puzzle #'h-manhattan-distance))
	(format t "Solution found in ~d moves~%" (- (length SolutionPath) 1))
	(format t "~d nodes generated (~d distinct nodes), ~d nodes expanded~%~%" *GENERATED* *DISTINCT* *EXPANDED*)
	(printSolution SolutionPath)

	; Use an A* search with the inadmissable heuristic manhattan distance squared
	(format t "A* graph search (Heuristic: Manhattan Distance Squared) ~%")
	(format t "--------------- ~%")
	(setf SolutionPath (A* Puzzle #'h-manhattan-squared))
	(format t "Solution found in ~d moves~%" (- (length SolutionPath) 1))
	(format t "~d nodes generated (~d distinct nodes), ~d nodes expanded~%~%" *GENERATED* *DISTINCT* *EXPANDED*)
	(printSolution SolutionPath)
)

(when (= (length *ARGS*) 1)
	(8puzzle *ARGS*)
)
