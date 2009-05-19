(let
	(assign 'ff
		(lambda 'n 'acc 'f_ '(if (< n '2)
			'(return acc)
			'(apply (read 'f_) (- n '1) (* acc n) '(read 'f_))
		)))
	(apply (read 'ff) '5 '1 '(read 'ff))
)
