(let
(assign 'xx '2)
(assign 'nn '3)

(assign 'fact (lambda 'n 'acc 'fact_
 '(if (< n '2)
     '(return acc)
     '(apply (read fact_) (- n '1) (* acc n) fact_ )
     )
 ))

(assign 'pow (lambda 'x 'y 'acc 'pow_
  '(if (< y '2)
    '(if (< y '1)
        '1
  	'(return acc)
    )
  	'(apply (read pow_) x (- y '1) (* acc x) pow_ )
  	)
))

 (assign 'sinterm (lambda 'x 'n 'pow_ 'fact_
'(let
 '(assign 'sign (apply (read pow_) '-1 n '-1 pow_ ))
 '(assign 'denom (apply (read pow_) x (+ '1 (* '2 n)) x pow_ ))
 '(assign 'numer (apply (read fact_) (+ '1 (* '2 n)) '1 fact_ ))
 '(* (read 'sign) (/ (read 'denom) (read 'numer)))
 )
)) 
 
(assign 'sine (lambda 'x 'n 'acc 'sine_ 'sinterm_ 'p 'f
 '(if (< n '0)
    '(return acc)
    '(apply (read sine_) x (- n '1) (+ acc (apply (read sinterm_) x n p f )) sine_ sinterm_ p f )
    )
 ))

(apply (read 'sine) (read 'xx) (read 'nn) '0 'sine 'sinterm 'pow 'fact )
)
; SKIP Scheme. RES = 0.907936507936508
