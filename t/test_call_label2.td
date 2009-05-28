; to use a quoted label with let
; the us of call and assign is obviously very contrived
(let
    '(assign 'v '2)
    (begin '(label a (* '3 '7)))
    '(assign 'w 'a)
    '(* (read 'v) (call w))
) 
