(let
     (buffer 1 '(rnd ))
    '(buffer 2 '(* 10 (stream 1)))
    '(buffer 3 '(+ 1000 (stream 2)))
    '(iter 10
        '(stream 3)
     )
)
