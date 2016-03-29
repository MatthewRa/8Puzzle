#|
                    ***** DFID.lsp *****

Author: Alex Herman
Written Spring 2016 for CSC447/547 AI class.

Description: This file includes an implementation of a Depth First Search using
	iterated deepening. It can be called using the command "(DFID puz)" where puz
	is a list of numbers that represents a puzzle. This file can be used with any
	n * n puzzle, but the file PuzzleFuncs.lsp must implement the functions MoveBlankUp,
	MoveBlankDown, MoveBlankLeft, MoveBlankRight, and IsSolved. The movement functions
	must also return NIL if the move is not valid.

	This file also implements a depth first search that can be used without
	iterated deepening. The syntax here is to call "(DFS puz bound)" where bound
	is large enough that the solution will never be found below that depth.

|#

;--------------------------------------------------------------------------

(load 'PuzzleFuncs)
; Global variable definition
(defvar *OPEN* nil)
(defvar *CLOSED* nil)

(defun DFID (lst)
	"This function calls a depth bounded DFS with an increasing depth bound"
	(let ((solution NIL))
		; Reset global counters
		(setf *GENERATED* 0)
		(setf *DISTINCT* 0)
		(setf *EXPANDED* 0)

		; Call DFS, iterating the depth bound until a solution is found
		(do ((x 1 (1+ x))) ((not (null solution)) solution)
			(setf *OPEN* nil)
			(setf *CLOSED* nil)
			(setf solution (reverse (DFS lst x)))

			; Track the number of distinct nodes generated during that trial
			(setf *DISTINCT* (+ *DISTINCT* (length *OPEN*)))
			(setf *DISTINCT* (+ *DISTINCT* (length *CLOSED*)))
		)
	)
)

(defun DFS (root bound)
	"This function implements a depth first search. If it reaches the specified
	depth, then the search will stop and return NIL."
	(let ((found nil) (current nil) (depth 0))
		; Initialize the OPEN list to contain the starting puzzle
		(push (list root nil) *OPEN*)

		; Do until a solution is found or until the OPEN list is empty
		(do () ((or found (null *OPEN*)) found)
			; Get the next node off the OPEN list
			(setf current (pop *OPEN*))

			; Find the depth of the current node
			(if (car (cdr current)) (setf depth (length current)) (setf depth 1))

			; If the node is within the depth bound, check if it is the solution
			; and expand its successors.
			(when (<= depth bound)
				(if (IsSolved (car current)) (setf found current))
				(incf *EXPANDED*)
				(DFIDGenerateSuccessors current)
			)

			; Once it is used, put it on the CLOSED list
			(push (car current) *CLOSED*)
		)
	)
)

(defun DFIDGenerateSuccessors (node)
	"This function pushes each valid successor onto the OPEN list"
	(let ((newNode nil))
		; For the 4 possible operations, do the following:
		; 1. Get the next node
		; 2. If it is a valid node and not on the CLOSED list, push it to OPEN
		(setf newNode (MoveBlankDown (car node)))
		(if newNode (incf *generated*))
		(when (and (not (null newNode)) (not (position newNode *CLOSED* :test #'equal))) (push (makeList node newNode) *OPEN*))
		(setf newNode (MoveBlankLeft (car node)))
		(if newNode (incf *generated*))
		(when (and (not (null newNode)) (not (position newNode *CLOSED* :test #'equal))) (push (makeList node newNode) *OPEN*))
		(setf newNode (MoveBlankUp (car node)))
		(if newNode (incf *generated*))
		(when (and (not (null newNode)) (not (position newNode *CLOSED* :test #'equal))) (push (makeList node newNode) *OPEN*))
		(setf newNode (MoveBlankRight (car node)))
		(if newNode (incf *generated*))
		(when (and (not (null newNode)) (not (position newNode *CLOSED* :test #'equal))) (push (makeList node newNode) *OPEN*))
	)
)

(defun makeList (lst newNode)
	"This function creates a list that contains the path from the current node to the root."
	(let ((returnList nil))
		; Get the current path to the parent not including any NIL values
		(dolist (item lst returnList) (if item (push item returnList)))
		(setf returnList (reverse returnList))
		; Push the new node to the beginning of the list
		(push newNode returnList)
	)
)
