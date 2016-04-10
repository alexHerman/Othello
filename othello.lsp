;(load 'minimax)
(load (merge-pathnames "minimax.lsp" *load-truename*))

(defstruct point x y)

(defun printBoard (board)
	"Prints the board to the string with column and row numbers"
	(format t "  ~D ~D ~D ~D ~D ~D ~D ~D~%" 1 2 3 4 5 6 7 8)

	(do ((y 1 (1+ y))) ((> y 8) 'T)
		(format t "~D " y)
		(do ((x 1 (1+ x))) ((> x 8) 'T)
			(format t "~D " (getValue board x y))
		)
		(format t "~%")
	)
)

(defun getValue (board x y)
	"Get the value of an x, y coordinate on the given board"
	(cond
		((and (> x 0) (> y 0)) (nth  (1- x) (nth (1- y) board)))
		(T NIL)
	)
)

(defun setValue (board x y val)
	"Set the value of an x, y coordinate on the given board"
	(let ((newBoard (deepCopy board)))
		(setf (nth  (1- x) (nth (1- y) newBoard)) val)
		newBoard
	)
)

(defun deepCopy (board)
	"Create a copy of a board"
	(let ((newBoard))
		(do ((y 0 (1+ y))) ((>= y 8) newBoard)
			(setf newBoard (append newBoard (list (copy-list (nth y board)))))
		)
	)
)

(defun generateSuccessors (board color)
	(let ((openPos) (x) (y))
		(do ((y 1 (1+ y))) ((> y 8) 'T)
			(do ((x 1 (1+ x))) ((> x 8) 'T)
				(if (and (equalp (getValue board x y) '-) (validMove board color x y) )
					(setf openPos (append openPos (list (make-point :x x :y y))))
				)
			)
		)
		openPos
	)
)

(defun generateSuccessorsNEW (board color)
	(let ((x) (y) (successors))
		(do ((y 1 (1+ y))) ((> y 8) 'T)
			(do ((x 1 (1+ x))) ((> x 8) 'T)
				(when (validMove board color x y)
					(setf successors (append successors (list (setValue board x y color))))
				)
			)
		)
		successors
	)
)

(defun gameOver (board)
	"Determines whether or not the game has ended. If it has, it returns the winner"
	(let ((black 0) (white 0))
		(cond
			; If there are no more available moves
			((and (null (generateSuccessors board 'W)) (null (generateSuccessors board 'B)))
				; Find the winner of the game
				(do ((y 1 (1+ y))) ((> y 8) 'T)
					(do ((x 1 (1+ x))) ((> x 8) 'T)
						(if (equal (getValue board x y) 'W) (setf white (1+ white)))
						(if (equal (getValue board x y) 'B) (setf black (1+ black)))
					)
				)
				; Return the winner, or a tie if relevant
				(cond
					((> black white) 'B)
					((> white black) 'W)
					(T 'TIE)
				)
			)
			; The game has not ended
			(T NIL)
		)
	)
)

(defun validMove (board player x y)
	"Determines if a certain x, y, position is a legal move for the given color"
	(cond
		; If the position does not already contain a piece
		((equalp (getValue board x y) '-)
			; Test every direction to find if this is a valid move
			(or
				(testDirection board player x y #'1- #'eval)
				(testDirection board player x y #'1+ #'eval)
				(testDirection board player x y #'eval #'1+)
				(testDirection board player x y #'eval #'1-)
				(testDirection board player x y #'1- #'1+)
				(testDirection board player x y #'1- #'1-)
				(testDirection board player x y #'1+ #'1+)
				(testDirection board player x y #'1+ #'1-)
			)
		)
		; NIL if the position is already filled
		(T NIL)
	)
)

(defun testDirection (board player x y xMove yMove)
	"Find if the new move captures any opponent pieces in a certain direction"
	(let ((opponentEncountered NIL) (leaveLoop NIL) (retVal NIL) (val NIL))
		(do () (leaveLoop retVal)
			(setf x (funcall xMove x))
			(setf y (funcall yMove y))
			(setf val (getValue board x y))
			(cond
				((equal val player)
					(if opponentEncountered (setf retVal T))
					(setf leaveLoop T)
				)
				((equal val '-) (setf leaveLoop T))
				((equal val NIL) (setf leaveLoop T))
				(T (setf opponentEncountered T))
			)
		)
	)
)

(setf start '((- - - - - - - -)
			  (- - - - - - - -)
			  (- - - - - - - -)
			  (- - - W B - - -)
			  (- - - B W - - -)
			  (- - - - - - - -)
			  (- - - - - - - -)
			  (- - - - - - - -)))
