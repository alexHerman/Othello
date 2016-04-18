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
;(load 'othello)
(load (merge-pathnames "othello.lsp" *load-truename*))
(setf beta -1000)

(defun minimax-withprint (position depth player)
	; if we have searched deep enough, or there are no successors,
	; return position evaluation and nil for the path
	(if (or (deepenough depth) (gameOver position))
		(list (static position) (list position))

    ; otherwise, generate successors and run minimax recursively
		(let
			(
				; generate list of sucessor positions
				(successors (generateSuccessors position player))

				; initialize current best path to nil
				(best-path nil)

				; initialize current best score to negative infinity
				(best-score -1000000)

				(turn player)

				; other local variables
				successor-value
				successor-score
			)
			(format t "Depth: ~D Player: ~A ~%" depth player)
			(PrintBoard position)
			(if (equal turn 'W) (setf turn 'B) (setf turn 'W))

			; explore possible moves by looping through successor positions
			(cond
				(successors
					(dolist (successor successors)

						; perform recursive DFS exploration of game tree
						(setq successor-value (minimax successor (1- depth) turn))


						; change sign every ply to reflect alternating selection
						; of MAX/MIN player (maximum/minimum value)
						(setq successor-score (- (car successor-value)))

						; update best value and path if a better move is found
						; (note that path is being stored in reverse order)
						(when (> successor-score best-score)
							(setq best-score successor-score)
							(setq best-path (append (list successor) (cadr successor-value)))
							; (setq best-path (cons successor (cdr successor-value)))
						)

						(format t "Depth: ~D Player: ~A Successor Score: ~D Best Score: ~D Beta: ~D~%"(1- depth) turn successor-score best-score beta)
						(PrintBoard successor)
						(when (and (= depth 1)(< (- successor-score) beta) )
							(format t "Pruned branch.~%")
							(return)
						)
						(when (= depth 2)
						(setf beta best-score))
						(when (> depth 2)
						(setf beta -1000))
					)
					(list best-score best-path)

				)
				(T (list (static position) (list position)))
			)


			; return (value path) list when done
		)
	)

)



(defun minimax (position depth player)
	; if we have searched deep enough, or there are no successors,
	; return position evaluation and nil for the path
	(if (or (deepenough depth) (gameOver position))
		(list (static position) (list position))

    ; otherwise, generate successors and run minimax recursively
		(let
			(
				; generate list of sucessor positions
				(successors (generateSuccessors position player))

				; initialize current best path to nil
				(best-path nil)

				; initialize current best score to negative infinity
				(best-score -1000000)

				(turn player)

				; other local variables
				successor-value
				successor-score
			)
			(if (equal turn 'W) (setf turn 'B) (setf turn 'W))

			; explore possible moves by looping through successor positions
			(cond
				(successors
					(dolist (successor successors)

						; perform recursive DFS exploration of game tree
						(setq successor-value (minimax successor (1- depth) turn))


						; change sign every ply to reflect alternating selection
						; of MAX/MIN player (maximum/minimum value)
						(setq successor-score (- (car successor-value)))

						; update best value and path if a better move is found
						; (note that path is being stored in reverse order)
						(when (> successor-score best-score)
							(setq best-score successor-score)
							(setq best-path (append (list successor) (cadr successor-value)))
							; (setq best-path (cons successor (cdr successor-value)))
						)
						(when (and (= depth 1)(< (- successor-score) beta) )
							(return)
						)
						(when (= depth 2)
						(setf beta best-score))
						(when (> depth 2)
						(setf beta -1000))
					)
					(list best-score best-path)

				)
				(T (list (static position) (list position)))
			)
		)
	)
)

(defun makemove (board player depth)
	(car (cadr (minimax board depth player)))
)

(defun testAI ()
	(let ((current start) (turn 'B) (test))
		(printBoard current)
		(do () ((gameOver current) (gameOver current))
			(if (equal turn 'W) (setf turn 'B) (setf turn 'W))
			(setf test (minimax current 3 turn))
			(setf current (car (cadr test)))
			(printBoard current)
		)
	)
)

(defun testHuman ()
	(let ((playerColor) (turn) (path) (row) (col) (current) (temp))
		(print "Do you want to go first [y/n]?: ")
		(if (equal (read) 'y) (setf playerColor 'B) (setf playerColor 'W))
		(if (equal playerColor 'W) (setf turn 'B) (setf turn 'W))
		(setf current error)
		(printBoard current)

		(cond
			((equal playerColor 'B)
				(do () ((gameOver current) (gameOver current))
					(format t "What is your move [row col]: ")
					(setf row (read))
					(setf col (read))
					(setf temp (validMove current playerColor col row))
					(if temp
						(setf current temp)
						(format t "That is an invalid move, you have forfeit your turn")
					)
					(printBoard current)
					(when (null (gameOver current))
						(setf path (minimax current 3 turn))
						(setf current (car (cadr path)))
					)
					(printBoard current)
				)
			)
			(T
				(do () ((gameOver current) (gameOver current))
					(setf path (minimax current 3 turn))
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
						(printBoard current)
					)
				)
			)
		)
	)
)




(defun test ()
	(printBoard (car (cadr (minimax-withprint errorTest 4 'B))))
)

(defun test-minimax (position depth player filename)
	;Because the output got too long for a command prompt...
	(with-open-file (*standard-output* (merge-pathnames filename *load-truename*) :direction :output :if-exists :supersede)
		(minimax-withprint position depth player)
	)
)

(setf errorTest '(W B - W - W W W B B B B B W W - W B W W W W W W W W B W W B W W W W W W B W B W W B W B W B B W B B B B B B B B W - - B B B B W))

;(test-minimax test1 1 'B "othello_test1-1.txt")
;(test-minimax test1 2 'B "othello_test1-2.txt")
;(test-minimax test1-W 2 'W "othello_test1-2-W.txt")
;(test-minimax test1 3 'B "othello_test1-3.txt")
;(test-minimax test2 2 'B "othello_test2.txt")
