(let
	(assign 'ff
		(lambda 'xv '(+ xv (* (+ xv '2) xv) (+ '0 '10)))) ;x+10+x*(x+2) = 12+10+14*12
(* '2	(apply (read 'ff) (* '2 '6))) ; 2*25=50
) ; should be 380

