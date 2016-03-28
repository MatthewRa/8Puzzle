(load 'PuzzleFuncs)
; Global variable definition
(defvar *OPEN* nil)
(defvar *CLOSED* nil)
(defvar *EXPANDED* 0)
(defvar *GENERATED* 0)
(defvar *DISTINCT* 0)

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

        (do () ((or found (null *OPEN*)) found)
            (setf current (pop *OPEN*))
            (push (car current) *CLOSED*)
            (if (car (cdr current)) (setf depth (length current)) (setf depth 1))
            (when (<= depth bound)
                (if (IsSolved (car current)) (setf found current))
                (incf *EXPANDED*)
                (GenerateSuccessors current)
            )
        )
    )
)

(defun GenerateSuccessors (node)
    (let ((newNode nil))
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
    (let ((returnList nil))
        (dolist (item lst returnList) (if item (push item returnList)))
        (setf returnList (reverse returnList))
        (push newNode returnList)
    )
)
