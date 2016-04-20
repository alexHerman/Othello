(defun minimax-withprint (position depth player)
	; if we have searched deep enough, or there are no successors,
	; return position evaluation and nil for the path
	(if (or (deepenough depth) (gameOver position))
		(list (static position player) (list (list position) NIL))

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
						(setq successor-value (minimax-withprint (car successor) (1- depth) turn))


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
						(format t "Move: ~a~%" (car (cdr successor)))
						;(PrintBoard (car successor))
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
				(T (list (static position player) (list (list position) NIL)))
			)


			; return (value path) list when done
		)
	)

)
(defun test-minimax (position depth player filename)
	;Because the output got too long for a command prompt...
	(let (beta -1000)
	(with-open-file (*standard-output* (merge-pathnames filename *load-truename*) :direction :output :if-exists :supersede)
		(minimax-withprint position depth player)
	)
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
;(test-minimax test1 2 'B "othello_test1-2.txt")
;(test-minimax test3 2 'B "othello_test3-2.txt")
;(test-minimax test3 2 'W "othello_test3-2-W.txt")
;(test-minimax test1-W 2 'W "othello_test1-2-W.txt")
;(test-minimax test1 3 'B "othello_test1-3.txt")
;(test-minimax test2 2 'B "othello_test2.txt")