(let
	(assign 'v '0)
	(if (< (read 'v) '2) ; true
		'(/ (+ (read 'v) '4) '2) ; (0+4)/2=2
		'(/ '4 (read 'v))
	) ; => 2
) ; => 2
