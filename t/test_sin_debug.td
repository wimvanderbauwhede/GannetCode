(let
(assign 'dow (lambda 'x 'y 'z 'p '(* '2 x)))
(assign 'pow (lambda 'x 'y 'acc 'pow_
  '(if (< y '2)
  	'(return acc)
  	'(apply (read 'pow_) x (- y '1) (* acc x) 'pow_ )
  	)
))

;'(assign 's (apply (read 'pow) '-1 '3 '-1 'dow))
'(*
(apply (read 'pow) '2 '7 '2 'pow)
'1
;(apply (read 'fact) '7 '1 'fact)
)

)
; SKIP Scheme. RES = 0.907936507936508
