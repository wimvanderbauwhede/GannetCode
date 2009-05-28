; Fact with accumulator
(let
	(assign 'fact
		(lambda 'n 'acc 'f_ '(if (< n '2)
			'(return acc)
			'(applytc (read f_) (- n '1) (* acc n) f_)
		)))
	'(apply fact '5 '1 'fact)
) ; => 1
