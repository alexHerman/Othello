(load 'minimax)

(defun printBoard (board)
	(format t "  ~D ~D ~D ~D ~D ~D ~D ~D~%" 1 2 3 4 5 6 7 8)

	(do ((x 1 (1+ x))) ((> x 8) 'T)
		(format t "~D " x)
		(do ((y 1 (1+ y))) ((> y 8) 'T)
			(format t "~D " (nth (+ (* (1- x)  8) (1- y)) board))
		)
		(format t "~%")
	)
)

(printBoard '(- - - - - - - -
			  - - - - - - - -
			  - - - - - - - -
			  - - - - - - - -
			  - - - - - - - -
			  - - - - - - - -
			  - - - - - - - -
			  - - - - - - - -))
