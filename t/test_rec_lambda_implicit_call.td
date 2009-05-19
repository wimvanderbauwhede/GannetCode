; Fact with accumulator. Currently, the compiler chokes on this!
(let
	(assign 'fact
		(lambda 'n 'acc '(if (< n '2)
			'(return acc)
			'(apply (read 'fact) (- n '1) (* acc n))
		)))
	(apply (read 'fact) '5 '1)
) ; => 120
