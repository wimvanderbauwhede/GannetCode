; Fact with accumulator
(let
	(assign 'fact
		(lambda 'n 'acc 'f_ '(if (< n '2)
			'(return acc)
			'(applyrec (read f_) (- n '1) (* acc n) f_)
		)))
	(apply (read 'fact) '5 '1 'fact)
) ; => 120
