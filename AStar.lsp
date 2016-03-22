(load "PuzzleFuncs.lsp")

(defun A* (node open closed)
	(let ( (newNode node) (newOpen open) (newClosed closed))
    (print newNode)
  	(cond
    	((correct (puz node)) node)
    	(t
    		(nconc newOpen (generateSuccessors node))
        (print newOpen)				
        (puz-push node newClosed)
        (print newClosed)
				(setf newNode (puz-pop newOpen))
				(A* newNode newOpen newClosed)
    	)
  	)
	)
)

(defun puz-top (list)
  (car (cdr list)) 
)

(defun puz-pop (list)
	(let (return)
		(setf return (car list))
  	(setf list (cdr list))
		return
	)
)

(defun puz-push (node list)
  (setf list (cons node (list list)))
)

(defun puz-node (parent list)
  (if (null parent) 
    (cons 0 (cons parent list))
    (cons (+ 1 (depth parent)) (cons parent list))  
  )
)

(defun correct (list)
  (equal list '(1 2 3 8 0 4 7 6 5 ))
)

(defun depth (list)
  (car list)
)

(defun puz (list)
  (cddr list)
)

(defun parent (list)
  (cadr list)
)

(defun result (node)
	(cond
		((null(parent node)) (cons (puz node) nil))	
		(t (append (cons (puz node) nil) (result (parent node)) ))
	)
)

(defun generateSuccessors (node)
  (let ( (newPuzzle nil) ( nodeList nil ) )
    (setf newPuzzle (MoveBlankUp (puz node)))
    (if newPuzzle (setf nodeList (cons (cons node newPuzzle) nodeList)))
    (setf newPuzzle (MoveBlankRight (puz node)))
    (if newPuzzle (setf nodeList (cons (cons node newPuzzle) nodeList)))
    (setf newPuzzle (MoveBlankDown (puz node)))
    (if newPuzzle (setf nodeList (cons (cons node newPuzzle) nodeList)))
    (setf newPuzzle (MoveBlankLeft (puz node)))
    (if newPuzzle (setf nodeList (cons (cons node newPuzzle) nodeList)))
		nodeList
	) 
)
