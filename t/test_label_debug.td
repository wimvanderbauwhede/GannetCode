(let
	(assign 'a '1)
		(if
			(> (read 'a) '4)
			'(read 'a)
			'(return '2 (update 'a (+ (read 'a) '1)))
		)
)
