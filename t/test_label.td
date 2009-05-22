(let
	'(assign 'a '1)
	'(label L 
		(if
			(> a '224) ; SUBTASK STACK limits it to about 128
			'(return a )
			'(return 'L (update 'a (+ a '1)))
		)
	)
)
