(let 
	(assign 'a2 2.0)
	(assign 'b3 2.0)
	(let
		(assign 'a5 (+ a2 3.0 (* 2.0 (let
					(assign 'b9 (+ 42.0 b3))
					(assign 'a11 (+ a2 4.0 (let (assign 'a14 (+ a2 5.0)) (* 2.0 a14))))
					(+ a11 b9 5.0)
					))
			))
		(assign 'b18 (+ a5 1.0))
		(+ a5 b18 6.0 (let (assign 'a22 (+ a5 2.0)) (* a22 3.0)))
	) 
)
--If we disallow referencing within a scope:
(let 
	(assign 'a2 2.0)
	(assign 'b3 2.0)
	(let
		(assign 'a5 (+ a2 3.0 (* 2.0 (let
					(assign 'b9 (+ 42.0 b3))
					(assign 'a11 (+ a2 4.0 (let (assign 'a14 (+ a2 5.0)) (* 2.0 a14))))
					(+ a11 b9 5.0)
					))
			))
		(assign 'b18 (+ a2 1.0))
		(+ a5 b18 6.0 (let (assign 'a22 (+ a5 2.0)) (* a22 3.0)))
	) 
)

