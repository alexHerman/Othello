#|*****************************LOADED FILES**********************************|#
(load (merge-pathnames "minimax.lsp" *load-truename*))
(load (merge-pathnames "minimax-functions.lsp" *load-truename*))
;(load (merge-pathnames "test-minimax.lsp" *load-truename*))

#|******************************CONSTANTS************************************|#
;standard start position of board
(defconstant start '(
	- - - - - - - -
	- - - - - - - -
	- - - - - - - -
	- - - W B - - -
	- - - B W - - -
	- - - - - - - -
	- - - - - - - -
- - - - - - - -))

#|*****************************************************************************
Function:   make-move 
Author:     Hannah Aker and Alex Herman
Written Spring 2016 for CSC447/547 AI class.
Description: 		
This function uses the minimax function to decide which move to make.
*****************************************************************************|#
(defun make-move (board player depth)
	(cadr (caadr (minimax board depth player player -10000 10000)))
)
#|*****************************************************************************
Function:   othello 
Author:     Hannah Aker and Alex Herman
Written Spring 2016 for CSC447/547 AI class.
Description: 		
This function is the standard entry point for the program, allows the user to 
play against the computer. 
*****************************************************************************|#
(defun othello ( &optional playerColor)
	(let ((turn) (path) (row) (col) (current) (temp))
		(when (null playerColor)
			(format t "Which color would you like to play [Black/B/white/W]?: ")
			(setf playerColor (read))
			(if (or (equal playerColor 'B) (equal playerColor 'Black)) (setf playerColor 'B) (setf playerColor 'W))
		)
		(if (equal playerColor 'W) (setf turn 'B) (setf turn 'W))
		(setf current start)
		
		(cond
			((equal playerColor 'B)
				(format t "~%OK! You will be playing Black. When asked for your move, please enter the row and column in which you would like to place a Black stone. Remember, you must outflank at least one White stone, or forfeit your move.~%~%")
				(printBoard current)
				(do () (
					(setf stats (gameOver current)) 
					(cond
						((> (car stats) (cadr stats)) (format t "Black wins! Black's Score: ~D White's Score: ~D~%" (car stats) (cadr stats)) 'B)
						((> (cadr stats) (car stats)) (format t "White wins! Black's Score: ~D White's Score: ~D~%" (car stats) (cadr stats)) 'W)
						(T (format t "TIE! Black's Score: ~D White's Score: ~D~%" (car stats) (cadr stats)) 'TIE)
					))
					(format t "What is your move [row col]: ")
					(setf row (read))
					(setf col (read))
					(setf temp (validMove current playerColor col row))
					(if temp
						(setf current temp)
						(format t "That is an invalid move, you have forfeit your turn.~%")
					)
					(format t "~%")
					(printBoard current)
					(when (null (gameOver current))
						
						(setf path (minimax current 4 turn turn -10000 10000))
						(setf current (caaadr path))
						(format t "Here is my move: ~a~%~%" (cadr (caadr path)))
					)
					(printBoard current)
				)
			)
			(T
				(format t "~%OK! You will be playing White. When asked for your move, please enter the row and column in which you would like to place a White stone. Remember, you must outflank at least one Black stone, or forfeit your move.~%~%")
				(printBoard current)
				(do () (
					(setf stats (gameOver current)) 
					(cond
						((> (car stats) (cadr stats)) (format t "Black wins! Black's Score: ~D White's Score: ~D~%" (car stats) (cadr stats)) 'B)
						((> (cadr stats) (car stats)) (format t "White wins! Black's Score: ~D White's Score: ~D~%" (car stats) (cadr stats)) 'W)
						(T (format t "TIE! Black's Score: ~D White's Score: ~D~%" (car stats) (cadr stats)) 'TIE)
					))
					
					(setf path (minimax current 4 turn turn -10000 10000))
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
							(format t "That is an invalid move, you have forfeit your turn.~%")
						)
						(format t "~%")
						(printBoard current)
					)
				)
			)
		)
	)
)


#|*****************************************************************************
Function:   othello-init 
Author:     Hannah Aker and Alex Herman
Written Spring 2016 for CSC447/547 AI class.
Description: 		
Empty function for othello initilization statements.
*****************************************************************************|#
(defun othello-init () )
#|*****************************************************************************
Function:   AIvsAI 
Author:     Hannah Aker and Alex Herman
Written Spring 2016 for CSC447/547 AI class.
Description: 		
Plays the AI against itself.
*****************************************************************************|#
(defun AIvsAI ()
	(let ((current start) (turn 'W) (test))
		(printBoard current)
		(do () (
			(setf stats (gameOver current)) 
			(cond
				((> (car stats) (cadr stats)) (format t "Black wins! Black's Score: ~D White's Score: ~D~%" (car stats) (cadr stats)) 'B)
				((> (cadr stats) (car stats)) (format t "White wins! Black's Score: ~D White's Score: ~D~%" (car stats) (cadr stats)) 'W)
				(T (format t "TIE! Black's Score: ~D White's Score: ~D~%" (car stats) (cadr stats)) 'TIE)
			))
			(if (equal turn 'W) (setf turn 'B) (setf turn 'W))
			
			(setf test (minimax current 2 turn turn -10000 10000))
			(setf current (caaadr test))
			(printBoard current)
		)
	)
	(values)
)


#|*****************************************************************************
Function:   printBoard 
Author:     Hannah Aker and Alex Herman
Written Spring 2016 for CSC447/547 AI class.
Description: 		
This function prints the board to a string with column and row numbers. 
*****************************************************************************|#
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

(othello *args*)




