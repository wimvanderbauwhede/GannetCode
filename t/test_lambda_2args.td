(let
	(assign 'ff
		(lambda 'xv 'yv '(+ xv (* (+ xv '2) yv) '10)))
 (+	
(* '3	(apply (read 'ff) (- '4 '2) (* '2 '10))) ; 3*32=96
(* '2	(apply (read 'ff) (- '4 '1) (* '2 '6))) ; 2*25=50
'10
) ; should be 156
)
