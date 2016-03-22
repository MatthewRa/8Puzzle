(load 'PuzzleFuncs)

; Node structure: stores state and parent.
(defstruct node state parent)

; Test if two nodes have the same state.
(defun equal-states (n1 n2) (equal (node-state n1) (node-state n2)))

;--------------------------------------------------------------------------

; Breadth-first-search implements the OPEN list as a QUEUE of (state parent) nodes.
(defun bfs (start) (SearchBfs start 'bfs))

; Depth-first-search implements the OPEN list as a STACK of (state parent) nodes.
(defun dfs (start) (SearchBfs start 'dfs))

; Given a start state (BFS) return a path from the start to the goal.
(defun SearchBfs (start type)
    (do*                                                    ; note use of sequential DO*
        (                                                   ; initialize local loop vars
            (curNode (make-node :state start :parent nil))  ; current node: (start nil)
            (OPEN (list curNode))                           ; OPEN list:    ((start nil))
            (CLOSED nil)                                    ; CLOSED list:  ( )
        )

        ; termination condition - return solution path when goal is found
        ((goal-state? (node-state curNode)) (build-solution curNode CLOSED))

        ; loop body
        (when (null OPEN) (return nil))             ; no solution

        ; get current node from OPEN, update OPEN and CLOSED
        (setf curNode (car OPEN))
        (setf OPEN (cdr OPEN))
        (setf CLOSED (cons curNode CLOSED))

        ; add successors of current node to OPEN
        (dolist (child (generate-successors (node-state curNode)))

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