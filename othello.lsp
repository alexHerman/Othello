; Gives certain locations greater value than others
(defconstant staticWeights '(99 -8 8 6 6 8 -8 99
	-8 -24 -4 -3 -3 -4 -24 -8
	8 -4 7 4 4 7 -4 8
	6 -3 4 0 0 4 -3 6
	6 -3 4 0 0 4 -3 6
	8 -4 7 4 4 7 -4 8
	-8 -24 -4 -3 -3 -4 -24 -8
	99 -8 8 6 6 8 -8 99))

(defun deepenough (depth)
	"Returns true if the search has gone down to the specified ply"
	(<= depth 0)
)

(defun static (position player)
	"Evaluates the current board. A high positive number is good for the max player, and a high negative is good for the min player"
	(let ((black 0) (white 0) (temp))
		(cond
			; If max has won, return infinity
			((equal (gameOver position) 'B)
				(if (equal player 'B) 10000 -10000)
			)
			; If min has won, return negative infinity
			((equal (gameOver position) 'W)
				(if (equal player 'B) -10000 10000)
			)
			; Calculates a point total for each player, max and min. The number of points that
			; a specific location is worth can be seen in the staticWeights table below
			(T
				(do ((y 1 (1+ y))) ((> y 8) 'T)
					(do ((x 1 (1+ x))) ((> x 8) 'T)
						(setf temp (getValue staticWeights x y))
						(if (equal (getValue position x y) 'B) (setf black (+ black temp)))
						(if (equal (getValue position x y) 'W) (setf white (+ white temp)))
					)
				)
				(if (equal player 'B) (- black white) (- white black))
			)
		)
	)
)


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

(defun hasSuccessor (board color)
	"Returns true if the given player has any successors for the given board."
	(let ((x) (y) (newBoard))
		; Return true after the first valid successor is found
		(do ((y 1 (1+ y))) ((or (> y 8) newBoard) newBoard)
			(do ((x 1 (1+ x))) ((or (> x 8) newBoard) newBoard)
				(setf newBoard (validMove board color x y))
			)
		)
	)
)

(defun generateSuccessors (board color)
	"Returns a list of successors for the given board"
	(let ((x) (y) (successors) (newBoard))
		; Check every poisition on the board
		(do ((y 1 (1+ y))) ((> y 8) 'T)
			(do ((x 1 (1+ x))) ((> x 8) 'T)
				; Get the new board. Will be NIL if position is invalid
				(setf newBoard (validMove board color x y))
				(when newBoard
					; Add the new board and the position of the new stone to the list
					(setf successors (append successors (list (list newBoard (list y x)))))
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
			((and (null (hasSuccessor board 'W)) (null (hasSuccessor board 'B)))
				; Find the winner of the game
				(do ((y 1 (1+ y))) ((> y 8) 'T)
					(do ((x 1 (1+ x))) ((> x 8) 'T)
						(if (equal (getValue board x y) 'W) (setf white (1+ white)))
						(if (equal (getValue board x y) 'B) (setf black (1+ black)))
					)
				)
				; Return the winner, or a tie if if that is the case
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
				; Each direction will return the board with any pieces flipped over in the given direction
				; If any pieces were flipped, then that version of the new board is saved
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
	(let ((opponentEncountered) (leaveLoop) (val) (board (setValue newBoard x y player)))
		(do () (leaveLoop board)
			; Move x and y in the desired direction from the new piece to be set down
			(setf x (funcall xMove x))
			(setf y (funcall yMove y))
			; Get the value of the board at the new location
			(setf val (getValue board x y))

			(cond
				; Location contains piece of the same color as the current player
				((equal val player)
					(if (null opponentEncountered) (setf board NIL))
					(setf leaveLoop T)
				)
				; Location contains no pieces
				((or (equal val '-) (equal val NIL)) (setf board NIL) (setf leaveLoop T))
				; Location contains opponents piece
				(T (setf opponentEncountered T) (setf board (setValue board x y player)))
			)
		)
	)
)

(setf start '(- - - - - - - -
			  - - - - - - - -
			  - - - - - - - -
			  - - - W B - - -
			  - - - B W - - -
			  - - - - - - - -
			  - - - - - - - -
			  - - - - - - - -))
