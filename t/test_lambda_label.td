; Simple lambda
(let
'(label mult (lambda 'x 'y '(* x y)))
'(apply mult '6 '7) ; => 42
)
