#|
***** MINIMAX.LSP *****

Generalized recursive minimax routine.

Author: Dr. John M. Weiss
Class:	SDSM&T CSC447/547 Artificial Intelligence
Date: 	Spring 2016

Usage:    (minimax position depth)
where position is the position to be evaluated,
and depth is the search depth (number of plys).

Returns:  (value path)
where value is the backed-up value from evaluation of leaf nodes,
and path is the path to the desired leaf node.

Functions called:

(deepenough depth) -
predicate that returns T if the current position has reached
the desired search depth, NIL otherwise.

(move-generator position) -
generates successors to the position.

(static position) -
applies the static evaluation function to the position.

Note: these functions may need additional arguments.
|#
(load (merge-pathnames "othello.lsp" *load-truename*))
;(load (merge-pathnames "test-minimax.lsp" *load-truename*))
;(setf beta -1000)

(defun minimax (position depth player maxplayer alpha beta)
	; if we have searched deep enough, or there are no successors,
	; return position evaluation and nil for the path
	(if (or (deepenough depth) (gameOver position))
		(list (static position maxplayer) position alpha beta)
		
    ; otherwise, generate successors and run minimax recursively
		(let
			(
				; generate list of sucessor positions
				(positions (generateSuccessors position player))
				;store successor position
				(new-position)
				;store all info about successor 
				(successor) 
				; initialize current best path to nil
				(best-path nil)
				; initialize current best score to negative infinity
				(best-score -1000000)
				;current player's turn
				(turn player)
			)
			;change turn for successors
			(if (equal turn 'W) (setf turn 'B) (setf turn 'W))
			
			; explore possible moves by looping through successor positions
			(cond
				(positions					
					(dolist (new-position positions)
						
						(cond 
							;if MAX player
							((equal player maxplayer)
								;perform recursive DFS exploration of game tree, use new beta, maintain current alpha
								(setq successor (minimax (car new-position) (1- depth) turn maxplayer alpha 10000))
								;update alpha when successor value is greater than alpha (MAX)
								(when (<= alpha (car successor))
									(setf alpha (car successor))
								)
							)
							;otherwise, MIN player
							(T 
								;perform recursive DFS exploration of game tree, use new alpha, maintain current beta
								(setq successor (minimax (car new-position) (1- depth) turn maxplayer -10000 beta))
								;update beta when successor value is less than beta (MIN)
								(when (>= beta (car successor))
									(setf beta (car successor))
								)
							)
						)
						; update best value and path if a better move is found
						; change successor score sign every ply to reflect alternating selection of MAX/MIN player (maximum/minimum value)
						; (note that path is being stored in reverse order)
						(when (> (- (car successor)) best-score)
							(setf best-score (- (car successor)))
							(setf best-path  (append (list new-position) (cdr successor)))
						)
						
						
						;if beta less than or equal to alpha, break
						(when (<= beta alpha)
							(return)
						)
					)
					;return best path
					(list best-score best-path alpha beta)
				)
				;if no successors, return SEF
				(T (list (static position maxplayer) position alpha beta) )
			)
		)
	)
)


(defun make-move (board player depth)
	
	(cadr (caadr (minimax board depth player player -10000 10000)))
	
)

(defun othello ()
	(let ((current start) (turn 'W) (test))
		(printBoard current)
		(do () ((gameOver current) (gameOver current))
			(if (equal turn 'W) (setf turn 'B) (setf turn 'W))
			(setf beta -1000)
			(setf test (minimax current 3 turn))
			(setf current (caaadr test))
			(printBoard current)
		)
	)
)

(defun testHuman ()
	(let ((playerColor) (turn) (path) (row) (col) (current) (temp))
		(format t "Do you want to go first [y/n]?: ")
	(if (equal (read) 'y) (setf playerColor 'B) (setf playerColor 'W))
	(if (equal playerColor 'W) (setf turn 'B) (setf turn 'W))
	(setf current start)
	
	(cond
		((equal playerColor 'B)
			(format t "~%OK! You will be playing Black. When asked for your move, please enter the row and column in which you would like to place a Black stone. Remember, you must outflank at least one White stone, or forfeit your move.~%~%")
			(printBoard current)
			(do () ((gameOver current) (gameOver current))
				(format t "What is your move [row col]: ")
				(setf row (read))
				(setf col (read))
				(setf temp (validMove current playerColor col row))
				(if temp
					(setf current temp)
					(format t "That is an invalid move, you have forfeit your turn")
				)
				(format t "~%")
				(printBoard current)
				(when (null (gameOver current))
					(setf beta -1000)
					(setf path (minimax current 4 turn))
					(setf current (caaadr path))
					(format t "Here is my move: ~a~%~%" (cadr (caadr path)))
				)
				(printBoard current)
			)
		)
		(T
			(format t "~%OK! You will be playing White. When asked for your move, please enter the row and column in which you would like to place a White stone. Remember, you must outflank at least one Black stone, or forfeit your move.~%~%")
			(printBoard current)
			(do () ((gameOver current) (gameOver current))
				(setf beta -1000)
				(setf path (minimax current 4 turn))
				(format t "Here is my move: ~a~%~%" (cadr (caadr path)))
				(setf current (car (cadr path)))
				(printBoard current)
				(when (null (gameOver current))
					(format t "What is your move [row col]: ")
					(setf row (read))
					(setf col (read))
					(setf temp (validMove current playerColor col row))
					(if temp
						(setf current temp)
						(format t "That is an invalid move, you have forfeit your turn~%~%")
					)
					(format t "~%")
					(printBoard current)
				)
			)
		)
	)
)
)
