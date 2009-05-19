(let 
	(assign 'a (let (* 1 2)))
	(assign 'b (* 2 2))
	(let
		(assign 'a (+ (read 'a) 3 (* 2 (let
					(assign 'b (+ 42 (read 'b)))
					(assign 'a (+ (read 'a) 4 
						(let 
							(assign 'a (+ (read 'a) 5)) 
							(* 2 (read 'a)))))
						(+ (read 'a) (read 'b) 5)
					))
			))
		(assign 'b (+ a 1))
		(+ a b 6 (let 
			(assign 'a (+ a 2)) 
			(* a 3)))
	) 
)

