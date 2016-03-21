(load 'PuzzleFuncs)
(defvar *OPEN* nil)
(defvar *CLOSED* nil)
(defvar *SOLUTION* nil)
(defvar *EXPANDED* 0)
(defvar *GENERATED* 0)
(defvar *DISTINCT* 0)

; Node structure: stores state and parent.
(defstruct node state parent)

; Test if two nodes have the same state.
(defun equal-states (n1 n2) (equal (node-state n1) (node-state n2)))

;--------------------------------------------------------------------------

; Breadth-first-search implements the OPEN list as a QUEUE of (state parent) nodes.
(defun bfs (start) (SearchBfs start 'bfs))

; Method that generates successors for a node
(defun BFS_GenerateSuccessors (list)
    (let ((blankPosition (position 0 list)) (listc '()))
		
		(format t "Blank Position: ~D~%" blankPosition)
		(format t "List: ~A~%" listc)
		
		(when (> blankPosition 3) ; Can Move up
			(if (null listc) ; check if listc is nill before trying to add to it
				(setf listc (list (MoveBlankUp list))) ; then
				(setf listc (append listc (list (MoveBlankUp list)))) ;else
			)
		)
		
		(when (< blankPosition 6) ; Can Move Down 
			(if (null listc) ; check if listc is nill before trying to add to it
				(setf listc (list (MoveBlankDown list))) ; then
				(setf listc (append listc (list (MoveBlankDown list)))) ;else
			)
		)
		
		(when (not (= (mod (+ blankPosition 1) 3) 0)) ; Can Move Right
			(if (null listc) ; check if listc is nill before trying to add to it
				(setf listc (list (MoveBlankRight list))) ; then
				(setf listc (append listc (list (MoveBlankRight list)))) ;else
			)
		)
		
		(when (not (= (mod (+ blankPosition 1) 3) 1)) ; Can Move Left
			(if (null listc) ; check if listc is nill before trying to add to it
				(setf listc (list (MoveBlankLeft list))) ; then
				(setf listc (append listc (list (MoveBlankLeft list)))) ;else
			)
		)
		
		(format t "After List: ~A~%~%" listc)
    )
)

; Given a start state (BFS) return a path from the start to the goal.
(defun SearchBfs (start type)
    (do*                                                    ; note use of sequential DO*
        (                                                   ; initialize local loop vars
            (curNode (make-node :state start :parent nil))  ; current node: (start nil)
            (OPEN (list curNode))                           ; OPEN list:    ((start nil))
            (CLOSED nil)                                    ; CLOSED list:  ( )
        )

        ; termination condition - return solution path when goal is found
        ((IsSolved (node-state curNode)) (build-solution curNode CLOSED))

        ; loop body
        (when (null OPEN) (return nil))             ; no solution

        ; get current node from OPEN, update OPEN and CLOSED
        (setf curNode (car OPEN))
        (setf OPEN (cdr OPEN))
        (setf CLOSED (cons curNode CLOSED))
		
		; Keep track of number of expanded nodes
		(setf *Expanded* (+ *Expanded* 1))

        ; add successors of current node to OPEN
        (dolist (child (GenerateSuccessors (node-state curNode)))

            ; for each child node
            (setf child (make-node :state child :parent (node-state curNode)))

            ; if the node is not on OPEN or CLOSED
            (if (and (not (member child OPEN   :test #'equal-states))
                     (not (member child CLOSED :test #'equal-states)))

                ; add it to the OPEN list
                (cond

                    ; BFS - add to end of OPEN list (queue)
                    ((eq type 'bfs) (setf OPEN (append OPEN (list child))))
                )
            )
        )
    )
)

;--------------------------------------------------------------------------

; Build-solution takes a state and a list of (state parent) pairs
; and constructs the list of states that led to the current state
; by tracing back through the parents to the start node (nil parent).
(defun build-solution (node node-list)
    (do
        ((path (list (node-state node))))        ; local loop var
        ((null (node-parent node)) path)         ; termination condition

        ; find the parent of the current node
        (setf node (member-state (node-parent node) node-list))

        ; add it to the path
        (setf path (cons (node-state node) path))
    )
)

; Member-state looks for a node on the node-list with the same state.
(defun member-state (state node-list)
    (dolist (node node-list)
        (when (equal state (node-state node)) (return node))
    )
)