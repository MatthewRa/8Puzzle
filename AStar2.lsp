(load "PuzzleFuncs.lsp")

(defun Ar* (node open closed h)
  ;(print (puz node))
  (cond
    ( (correct (puz node)) node )
    (t

      (if open (nconc open
        (generateSuccessors node h closed))
        (setf open (generateSuccessors node h closed))
      )
      (if closed
        (setf closed (cons node closed))
        (setf closed (list node))
      )
      (setf open (sort open #'< :key #'cadr))
      (setf node (car open))
      (setf open (cdr open))
      ;(print open)
      ;(break)
      (Ar* node open closed h)

    )
  )
)

(defun A* (puzzle h)
  (let
  	(
      (open nil)
      (closed nil)
      (node (puz-node nil puzzle h))
      (counter 0)
    )

    (loop while (and node (not (correct (puz node)))) do

      (if open (nconc open
        (generateSuccessors node h closed))
        (setf open (generateSuccessors node h closed))
      )
      (if closed
        (setf closed (cons node closed))
        (setf closed (list node))
      )
      (setf open (sort open #'< :key #'cadr))
      (setf node (car open))
      (setf open (cdr open))
      (setf counter (+ counter 1) )
      ;(print node)
    )
    (result node)
  )
)


(defun puz-node (parent list h)
  (if (null parent)
    (nconc (list 0 0 (append (puz parent) nil)) list)
    (nconc
      (list
        (+ 1 (depth parent))
        (+ (+ (depth parent) (funcall h list)) 1)
        (append (puz parent) nil)
      )
      list
    )
  )
)

(defun h-test (list)
  0
)

(defun h-misplaced-tiles (list)
  (h-m-t-internal list '(1 2 3 8 0 4 7 6 5 ))
)

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

(defun h-manhattan-squared (list)
  (let ((x (h-manhattan-distance list)))
    (* x x )
  )
)

(defun h-manhattan-distance (list)
  (let ( (final '(1 2 3 8 0 4 7 6 5 )) (dist 0) )
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

(defun correct (list)
  (equal list '(1 2 3 8 0 4 7 6 5 ))
)

(defun depth (list)
  (car list)
)

(defun distance (list)
  (cadr list)
)

(defun puz (list)
  (cdddr list)
)

(defun parent (list)
  (caddr list)
)

(defun result (node)
	(cond
		((null(parent node)) (cons (puz node) nil))
		(t (append (cons (puz node) nil) (result (parent node)) ))
	)
)

(defun contains_better (node closed)
  (let ( (other (contains (puz node) closed)) )
  (cond
    (other (>= (distance other) (distance node) ) )
    ( t nil )
  )
  )
)

(defun contains (list closed)
  (cond
    ( (null closed)   nil )
    ( (equal list (puz (car closed)))

      (car closed)
    )
    ( t (contains list (cdr closed)))
  )
)

(defun generateSuccessors (node h closed)
  (let ( (newPuzzle nil) ( nodeList nil ) (test nil))
    (setf newPuzzle (MoveBlankUp (puz node)))
    (if (and newPuzzle (not (contains (puz node) closed)))
      (setf nodeList (cons (puz-node node newPuzzle h) nodeList))
    )
    (setf newPuzzle (MoveBlankDown (puz node)))
    (if (and newPuzzle (not (contains (puz node) closed)))
      (setf nodeList (cons (puz-node node newPuzzle h) nodeList))
    )
    (setf newPuzzle (MoveBlankLeft (puz node)))
    (if (and newPuzzle (not (contains (puz node) closed)))
      (setf nodeList (cons (puz-node node newPuzzle h) nodeList))
    )
    (setf newPuzzle (MoveBlankRight (puz node)))
    (if (and newPuzzle (not (contains (puz node) closed)))
      (setf nodeList (cons (puz-node node newPuzzle h) nodeList))
    )
    nodeList
  )
)

(defun generateSuccessors (node h closed)
  (let ( (newPuzzle nil) ( nodeList nil ) (test nil))
    (setf newPuzzle (MoveBlankUp (puz node)))
    (if (and newPuzzle (not (contains_better newPuzzle closed)))
      (setf nodeList (cons (puz-node node newPuzzle h) nodeList))
    )
    (setf newPuzzle (MoveBlankDown (puz node)))
    (if (and newPuzzle (not (contains_better newPuzzle closed)))
      (setf nodeList (cons (puz-node node newPuzzle h) nodeList))
    )
    (setf newPuzzle (MoveBlankLeft (puz node)))
    (if (and newPuzzle (not (contains_better newPuzzle closed)))
      (setf nodeList (cons (puz-node node newPuzzle h) nodeList))
    )
    (setf newPuzzle (MoveBlankRight (puz node)))
    (if (and newPuzzle (not (contains_better newPuzzle closed)))
      (setf nodeList (cons (puz-node node newPuzzle h) nodeList))
    )
    nodeList
  )
)
