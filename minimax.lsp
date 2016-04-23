#|*****************************************************************************
Function:   minimax 
Author:     Hannah Aker and Alex Herman
Written Spring 2016 for CSC447/547 AI class.
Description: 		
This function performs the minimax algorithm with alpha-beta pruning by
recursively performing a depth first search to the depth provided. The best
score and path are updated whenever a better score is found. Alpha is 
updated when a successor returns a larger value on a MAX turn. Beta is
updated when a successor returns a smaller value on a MIN turn. If beta is
less than alpha, the branch is pruned.
*****************************************************************************|#
(defun minimax (position depth player maxplayer alpha beta)
	"This function performs the minimax algorithm with alpha-beta pruning by
	recursively performing a depth first search to the depth provided. The best
	score and path are updated whenever a better score is found. Alpha is 
	updated when a successor returns a larger value on a MAX turn. Beta is
	updated when a successor returns a smaller value on a MIN turn. If beta is
	less than alpha, the branch is pruned."
	; if we have searched deep enough, or there are no successors, return position evaluation
	(if (or (deepenough depth) (gameOver position))
		(list (static position maxplayer) (list (list position) NIL) alpha beta)
		
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
				(T (list (static position maxplayer) (list (list position) NIL) alpha beta) )
			)
		)
	)
)

