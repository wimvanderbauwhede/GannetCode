; recursive lambda with buffer. 
; this does not compile
(let
 	(buf 'f 
	 (lambda 'n 
	  '(if (< n '2) 
		  '(return n) 
		  '(applytc f (- n '1))
	   )
	 )
	)
	'(apply f '5)
)