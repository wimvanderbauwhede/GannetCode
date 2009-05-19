(let
	(assign 'acc '1)
	(assign 'n '1)
	'(label L 
		(if
			(> n '5)
			'(return acc )
			'(return 'L (update 'n (+ n '1)) (update 'acc (* n acc)))
		)
	)
)
