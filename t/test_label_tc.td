(let
	'(assign 'a '1)
	'(label L 
		(iftc
			(> a '300) ; SUBTASK STACK limits it to about 128
			'(return a )
			'(returntc 'L (update 'a (+ a '1)))
		)
	)
)
