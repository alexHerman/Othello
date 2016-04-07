;(load 'minimax)
(load (merge-pathnames "minimax.lsp" *load-truename*))

(defstruct point x y)

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

(defun validMove (board color x y)
	T
)

(setf start '((- - - - - - - -)
	(- - - - - - - -)
	(- - - - - - - -)
	(- - - W B - - -)
	(- - - B W - - -)
	(- - - - - - - -)
	(- - - - - - - -)
(- - - - - - - -)))
