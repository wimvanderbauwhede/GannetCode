; "Implicit" let vars don't work with Puffin
; because 'ff gets translated to 'ff' and f_ to $f_
; whereas f_ should be read($f)
; to make it worse, 'f_ becomes $f_ as well
(let
	(assign 'ff
		(lambda 'n 'acc 'f_ '(if (< n '2)
			'(return acc)
			'(apply f_ (- n '1) (* acc n) 'f_)
		)))
(apply ff '5 '1 'ff)
)
