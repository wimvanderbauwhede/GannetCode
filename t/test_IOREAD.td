(let
	(assign 'c '0)
	'(label L
		(if (> (read 'c) '4)
			'(return '1)
			'(return 'L (iowrite '2 (ioread '1)) (update 'c (+ (read 'c) '1)))
		)
	)
)
