(load 'minimax)

(defun printBoard (board)
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
	(nth  (1- x) (nth (1- y) board))
)

(printBoard '((- - - - - - - -)
			  (- - - - - - - -)
			  (- - - - - - - -)
			  (- - - - B - - -)
			  (- - - - - - - -)
			  (- - - - - - - -)
			  (- - - - - - - -)
			  (- - - - - - - -)))
