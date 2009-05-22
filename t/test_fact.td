(+ '0
(let
        (assign 'v '42)
	    (assign 'f (lambda 'n 'acc 'f_ '(if (< n '2) 
						'(return acc)
						'(applytc (read f_) (- n '1) (* acc n) f_)
					)
			)
    	)
    	'(apply (read 'f) '12 '1 'f)
)
)
