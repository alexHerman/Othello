(load (merge-pathnames "othello.lsp" *load-truename*))


(defun minimax-withprint (position depth player maxplayer alpha beta)
	; if we have searched deep enough, or there are no successors,
	; return position evaluation and nil for the path
	(if (or (deepenough depth) (gameOver position))
		(list (static position maxplayer) position alpha beta)
		
		; otherwise, generate successors and run minimax recursively
		(let
			(
				(positions (generateSuccessors position player))
				(new-position)
				(successor)
				(best-path nil)
				(turn player)
				(best-score -10000)
				
				
			)
			(format t "Depth: ~D Player: ~A Beta: ~D Alpha: ~D~%" depth turn  beta alpha)
			(PrintBoard position)
			(if (equal turn 'W) (setf turn 'B) (setf turn 'W))
			
			; explore possible moves by looping through successor positions
			(cond
				(positions					
					(dolist (new-position positions)
						(cond 
							((equal player maxplayer)
								(setq successor (minimax-withprint (car new-position) (1- depth) turn maxplayer alpha 10000))
								(when (<= alpha (car successor))
									(setf alpha (car successor))
								)
							)
							(T (setq successor (minimax-withprint (car new-position) (1- depth) turn maxplayer -10000 beta))
								(when (>= beta (car successor))
									(setf beta (car successor))
								)
							)
						)
						; update best value and path if a better move is found
						; (note that path is being stored in reverse order)
						(when (> (- (car successor)) best-score)
							(setf best-score (- (car successor)))
							(setf best-path  (append (list new-position) (cdr successor)))
							; (setq best-path (cons successor (cdr successor-value)))
						)
						
						(format t "Depth: ~D Player: ~A Move: ~A SEF: ~D Beta: ~D Alpha: ~D Best Score: ~D~%"(1- depth) turn (cadr new-position) (car successor) beta alpha best-score)
						
						;if beta less than or equal to alpha, break
						(when (<= beta alpha) 						
							(format t "Pruned branch.~%")
							(return)
						)
					)
					(list best-score best-path alpha beta)
				)
				(T (list (static position maxplayer) position alpha beta) )
				)
			)
		)
	)
	
	
	(defun test-minimax (position depth player filename)
		;Because the output got too long for a command prompt...
		(with-open-file (*standard-output* (merge-pathnames filename *load-truename*) :direction :output :if-exists :supersede)
			(setf best-path (minimax-withprint position depth player player -10000 10000))
			(format t "Best Path: ~a" best-path)
		)
	)
	
	(setf test1 '(
		- - - - - - - -
		- - - - - - - -
		- - - - - - - -
		- - - W B - - -
		- - - W W W W -
		- - - W - - - -
		- - - - - - - - 
	- - - - - - - -))
	
	(setf test1-W '(
		- - - - - - - -
		- - - - - - - -
		- - - - - - - -
		- - - B W - - -
		- - - B B B B -
		- - - B - - - -
		- - - - - - - - 
	- - - - - - - -))
	
	(setf test2 '(
		- - B B - - - -
		- - - B W B - -
		- W B B W W W B
		B B W W W B W W
		B W W W B B B W
		B W - W W - W B
		- - - W B - - - 
	- - - - B - - -))
	
	(setf test3 '(
		- - - - - - - - 
		- - - - - - - - 
		- - W W W W - - 
		- - - B B - - - 
		- - B W B B - - 
		- B W - - - - - 
		- - - - - - - - 
	- - - - - - - - ))
	
	;(test-minimax test1 1 'B "othello_test1-1.txt")
	(test-minimax test1 2 'B "othello_test1-2.txt")
	;(test-minimax test1 3 'B "othello_test1-3.txt")
	;(test-minimax test1 4 'B "othello_test1-4.txt")
	
	;(test-minimax test3 2 'B "othello_test3-2.txt")
	;(test-minimax test3 2 'W "othello_test3-2-W.txt")
	;(test-minimax test1-W 2 'W "othello_test1-2-W.txt")
;(test-minimax test2 2 'B "othello_test2.txt")		