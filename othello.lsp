(defstruct point x y)

(defun deepenough (depth)
	(<= depth 0)
)

(defun static (position)
	(let ((black 0) (white 0) (temp))
		(cond
			((null (generateSuccessorsNEW position 'B))
				-1000000
			)
			((null (generateSuccessorsNEW position 'W))
				1000000
			)
			(T
				(do ((y 1 (1+ y))) ((> y 8) 'T)
					(do ((x 1 (1+ x))) ((> x 8) 'T)
						(setf temp (getValue staticWeights x y))
						(if (equal (getValue position x y) 'B) (setf black (+ black temp)))
						(if (equal (getValue position x y) 'W) (setf white (+ white temp)))
					)
				)
				(- black white)
			)
		)
	)
)

(setf staticWeights '(99 -8 8 6 6 8 -8 99
					  -8 -24 -4 -3 -3 -4 -24 -8
					  8 -4 7 4 4 7 -4 8
					  6 -3 4 0 0 4 -3 6
					  6 -3 4 0 0 4 -3 6
					  8 -4 7 4 4 7 -4 8
					  -8 -24 -4 -3 -3 -4 -24 -8
					  99 -8 8 6 6 8 -8 99))

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
	(format t "~%")
)

(defun printBoardList (lst)
	(dolist (board lst)
		(printBoard board)
	)
)

(defun getValue (board x y)
	"Get the value of an x, y coordinate on the given board"
	(cond
		((and (> x 0) (> y 0) (< x 9) (< y 9)) (nth (+ (* (1- y) 8) (1- x)) board))
		(T NIL)
	)
)

(defun setValue (board x y val)
	"Set the value of an x, y coordinate on the given board"
	(let ((newBoard (copy-list board)))
		(setf (nth (+ (* (1- y) 8) (1- x)) newBoard) val)
		newBoard
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
	(let ((x) (y) (successors) (newBoard))
		(do ((y 1 (1+ y))) ((> y 8) 'T)
			(do ((x 1 (1+ x))) ((> x 8) 'T)
				(setf newBoard (validMove board color x y))
				(when newBoard
					(setf successors (append successors (list newBoard)))
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

(defun validMove (newboard player x y)
	"Determines if a certain x, y, position is a legal move for the given color"
	(let ((temp) (isValid nil))
		(cond
			; If the position does not already contain a piece
			((equalp (getValue newboard x y) '-)
				; Test every direction to find if this is a valid move
				(setf temp (testDirection newboard player x y #'1- #'eval))
				(when temp (setf isValid T) (setf newBoard temp))
				(setf temp (testDirection newboard player x y #'1+ #'eval))
				(when temp (setf isValid T) (setf newBoard temp))
				(setf temp (testDirection newboard player x y #'eval #'1-))
				(when temp (setf isValid T) (setf newBoard temp))
				(setf temp (testDirection newboard player x y #'eval #'1+))
				(when temp (setf isValid T) (setf newBoard temp))
				(setf temp (testDirection newboard player x y #'1- #'1+))
				(when temp (setf isValid T) (setf newBoard temp))
				(setf temp (testDirection newboard player x y #'1- #'1-))
				(when temp (setf isValid T) (setf newBoard temp))
				(setf temp (testDirection newboard player x y #'1+ #'1+))
				(when temp (setf isValid T) (setf newBoard temp))
				(setf temp (testDirection newboard player x y #'1+ #'1-))
				(when temp (setf isValid T) (setf newBoard temp))
				(if isValid newBoard)
			)
			; NIL if the position is already filled
			(T NIL)
		)
	)
)

(defun testDirection (newBoard player x y xMove yMove)
	"Find if the new move captures any opponent pieces in a certain direction"
	(let ((opponentEncountered NIL) (leaveLoop NIL) (val NIL) (board (setValue newBoard x y player)))
		(do () (leaveLoop board)
			(setf x (funcall xMove x))
			(setf y (funcall yMove y))
			(setf val (getValue board x y))
			(cond
				((equal val player)
					(if (null opponentEncountered) (setf board NIL))
					(setf leaveLoop T)
				)
				((or (equal val '-) (equal val NIL)) (setf board NIL) (setf leaveLoop T))
				(T (setf opponentEncountered T) (setf board (setValue board x y player)))
			)
		)
	)
)

(setf error '(- - - - - - - -
	- - - - - - - -
	- - - - - - - -
	- - - - W W B -
	- - - W W - - -
	- - - W - - - -
	- - - B - - - -
- - - - - - - -))
(setf start '(- - - - - - - -
	- - - - - - - -
	- - - - - - - -
	- - - W B - - -
	- - - B W - - -
	- - - - - - - -
	- - - - - - - -
- - - - - - - -))

(setf test1 '(- - - - - - - -
	- - - - - - - -
	- - - - - - - -
	- - - W B - - -
	- - - W W W W -
	- - - W - - - -
	- - - - - - - -
- - - - - - - -))

(setf test1-W '(- - - - - - - -
	- - - - - - - -
	- - - - - - - -
	- - - B W - - -
	- - - B B B B -
	- - - B - - - -
	- - - - - - - -
- - - - - - - -))
(setf test2 '(- - B B - - - -
	- - - B W B - -
	- W B B W W W B
	B B W W W B W W
	B W W W B B B W
	B W - W W - W B
	- - - W B - - -
- - - - B - - -))
