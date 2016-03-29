#|
  File: AStar.lsp
  Author: Marcus Haberling
  Description: A* algorithim and helper functions.
|#
(load "PuzzleFuncs.lsp")

#|
  Runs the A* algorith iteratively. While setting global statistic
  variables. Pass in the start puzzle and a heuristic function.
|#
(defun A* (puzzle h)
  (let
    (
      (open nil)
      (closed nil)
      (node (puz-node nil puzzle h))
    )
    ;zero out the globals
		(setf *EXPANDED* 0)
		(setf *GENERATED* 0)
		(setf *DISTINCT* 0)

    ;loop while their is a valid open node thats not the answer
    (loop while (and node (not (correct (puz node)))) do
      ;Add successors to open list (if is to handle empty open list)
      (if open (nconc open
        (A*GenerateSuccessors node h closed))
        (setf open (A*GenerateSuccessors node h closed))
      )

      ;move the current node onto the closed list
      (if closed
        (setf closed (cons node closed))
        (setf closed (list node))
      )

      ;we need to sort open by the estimated distance in case new nodes
      ; are better than oler ones.
      (setf open (sort open #'< :key #'cadr))
      ;set node to the next to be generated      
      (setf node (car open))
      ;pop that node off the list
      (setf open (cdr open))

    )
		(setf *DISTINCT* (+ (length open) (length closed) ) )
    (reverse (result node))
  )
)

#|
  Creates a list representing a node in the tree
  pos 1 is the depth of the parent node.
  pos 2 is the depth of the node plus its heuristic
  pos 3 is the parent node
  pos 4 and on is the actual node state
|#
(defun puz-node (parent list h)
  (if (null parent)
    (nconc (list 0 0 parent) list)
    (nconc
      (list
        (+ 1 (depth parent))
        (+ (+ (depth parent) (funcall h list)) 1)
        parent
      )
      list
    )
  )
)



(defun h-misplaced-tiles (list)
  (h-m-t-internal list '(1 2 3 8 0 4 7 6 5 ))
)

#|
  Heuristic: runs the internal recursive call for
  misplaced tiles.
|#
(defun h-m-t-internal (list1 list2)
  (cond
    ( (null list1)
      0
    )
    ( (= (car list1) (car list2) )
      (h-m-t-internal (cdr list1) (cdr list2) )
    )
    ( t
      (+ 1  (h-m-t-internal (cdr list1) (cdr list2)))
    )
  )
)

#|
  Heuristic: Returns the square of the manhatten distance.
  Originally started as just an expiriment to see what happens.
  Turned out to be a great example of an inadmissible heuristic.
  It runs the tougher puzzles much faster while giving a less than
  optimal solution
|#
(defun h-manhattan-squared (list)
  (let ((x (h-manhattan-distance list)))
    (* x x )
  )
)

#|
  Heuristic: Returns the sum of the distances for each node from their correct
  positions. does this by subtracting the differemces of the integer devision of
  the positions by 3 (to get row) and the differences of the modulus by three (to get col)
|#
(defun h-manhattan-distance (list)
  (let ( (final '(1 2 3 8 0 4 7 6 5 )) (dist 0) )
  (cond
    ( (null list)
      0
    )
    (t
      (loop for i in final do
        (setf dist
          (+ dist
            (abs (- (floor (/ (position i list) 3)) (floor (/ (position i final) 3)) ))
          )
        )
        (setf dist
          (+ dist
            (abs (- (floor (mod (position i list) 3)) (floor (mod (position i final) 3)) ))
          )
        )
      )
      dist
    )
    )
  )
)

#|
  Returns true if the list is in goal state
|#
(defun correct (list)
  (equal list '(1 2 3 8 0 4 7 6 5 ))
)

#|
  get depth of the node
|#
(defun depth (list)
  (car list)
)

#|
  get the total estimated distance from node
|#
(defun distance (list)
  (cadr list)
)

#|
  return the actual puzzle state from node
|#
(defun puz (list)
  (cdddr list)
)

#|
  return the parent of the node;
|#
(defun parent (list)
  (caddr list)
)

#|
  Prints out the list
|#
(defun result (node)
  (cond
    ((null(parent node)) (cons (puz node) nil))
    (t (append (cons (puz node) nil) (result (parent node)) ))
  )
)

#|
  returns true if the list is already on the closed list.
|#
(defun contains (list closed)
  (cond
    ( (null closed)   nil )
    ( (equal list (puz (car closed)))

      (car closed)
    )
    ( t (contains list (cdr closed)))
  )
)

#|
  Returns a list of successor nodes or a nil if none can be generated
  Also iterates the *Expanded* variable by one and the *Generated*
  for each node genereated regardless of if its added to the lis or not
|#
(defun A*GenerateSuccessors (node h closed)
  (let ( (newPuzzle nil) ( nodeList nil ) (test nil))
    (setf *EXPANDED* (+ 1 *EXPANDED*))
    (setf newPuzzle (MoveBlankUp (puz node)))
		(if newPuzzle (setf *GENERATED* (+ 1 *GENERATED*)))
    (if (and newPuzzle (not (contains (puz node) closed)))
      (setf nodeList (cons (puz-node node newPuzzle h) nodeList))
    )
    (setf newPuzzle (MoveBlankDown (puz node)))
		(if newPuzzle (setf *GENERATED* (+ 1 *GENERATED*)))
    (if (and newPuzzle (not (contains (puz node) closed)))
      (setf nodeList (cons (puz-node node newPuzzle h) nodeList))
    )
    (setf newPuzzle (MoveBlankLeft (puz node)))
		(if newPuzzle (setf *GENERATED* (+ 1 *GENERATED*)))
    (if (and newPuzzle (not (contains (puz node) closed)))
      (setf nodeList (cons (puz-node node newPuzzle h) nodeList))
    )
    (setf newPuzzle (MoveBlankRight (puz node)))
		(if newPuzzle (setf *GENERATED* (+ 1 *GENERATED*)))
    (if (and newPuzzle (not (contains (puz node) closed)))
      (setf nodeList (cons (puz-node node newPuzzle h) nodeList))
    )
    nodeList
  )
)
