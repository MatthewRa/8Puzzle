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
	"This function gets the starting puzzle from the file or the command line and
	then returns to the main function."
	(cond
		((> (length args) 1)
			(setf Puzzle args)
			(setf Puzzle (mapcar #'parse-integer Puzzle))
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
	(format t "Solution found in ~d moves~%" (- (length SolutionPath) 1))
	(format t "~d nodes generated (~d distinct nodes), ~d nodes expanded~%~%" *GENERATED* *DISTINCT* *EXPANDED*)

	(printSolution SolutionPath)

	(format t "DFID graph search ~%")
	(format t "----------------- ~%")
  (setf SolutionPath (DFID Puzzle))
	(format t "Solution found in ~d moves~%" (- (length SolutionPath) 1))
  (format t "~d nodes generated (~d distinct nodes), ~d nodes expanded~%~%" *GENERATED* *DISTINCT* *EXPANDED*)

	(printSolution SolutionPath)
	(format t "A* graph search ~%")
	(format t "--------------- ~%")
)

; call the main function, passing command-line arguments
(main *ARGS*)
