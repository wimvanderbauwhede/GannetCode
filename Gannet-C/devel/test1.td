; AProgram
(let
; Function Definition
    '(assign 'square  (lambda 
        'x 
        ; APureExpr: * x x 
        ; Operator Call
            (* 
            ; Variable:
                x 
            ; Variable:
                x 
            ) ; OpCall
        ) ; FunDef-lambda?
    ) ; Fundef-assign
; Procedure Definition
    (label fortytwo 
        (let
        ; Assignment 
            '(assign::int  'x 
            ; Number:
                '6 
            ) ; Assign
        ; Assignment 
            '(assign::int  'y 
            ; Number:
                '7 
            ) ; Assign
        ; APureExpr: * x y 
        ; Operator Call
            '(* 
            ; Variable:
                x 
            ; Variable:
                y 
            ) ; OpCall
        ) ; FunDef-lambda?
    ) ; Fundef-let
; Procedure Definition
    (label fortytwovoid 
        (return
        ; APureExpr: 42 
        ; Number:
            '42 
        ) ; FunDef-lambda?
    ) ; Fundef-return
; APureExpr: int a 6 Int b 7 s1 in 0 out 1 s2 in 0 out 1 + a * b s3 c 42 Stream int test < a b a += 1 s1 1 2 3 seq s2 3 2 1 square 5 seq 
; Parallel LET 12182618
    (let 
    ; Assignment 
        (assign::int  'a 
        ; Number:
            '6 
        ) ; Assign
    ; APureExpr: Int b 7 s1 in 0 out 1 
    ; Sequential LET
        (let
        ; Assignment 
            '(assign::Int  'b 
            ; Number:
                '7 
            ) ; Assign
        ; APureExpr: s1 in 0 out 1 
        ; Service Call
            '(s1 
            ; APureExpr: in 
            ; Procedure Call
                in 
            ; APureExpr: 0 
            ; Number:
                '0 
            ; APureExpr: out 1 
            ; Function Application
                (apply out 
                ; APureExpr: 1 
                ; Number:
                    '1 
                ) ; FunAppl
            ) ; FunAppl
        ) ; SeqLet
    ; APureExpr: s2 in 0 out 1 
    ; Service Call
        (s2 
        ; APureExpr: in 
        ; Procedure Call
            in 
        ; APureExpr: 0 
        ; Number:
            '0 
        ; APureExpr: out 1 
        ; Function Application
            (apply out 
            ; APureExpr: 1 
            ; Number:
                '1 
            ) ; FunAppl
        ) ; FunAppl
    ; APureExpr: + a * b s3 c 42 
    ; Operator Call
        (+ 
        ; Variable:
            a 
        ; Operator Call
            (* 
            ; Variable:
                b 
            ; Service Call
                (s3 
                ; APureExpr: c 
                ; Variable:
                    c 
                ; APureExpr: 42 
                ; Number:
                    '42 
                ) ; FunAppl
            ) ; OpCall
        ) ; OpCall
    ; Assignment 
        (assign::Stream int  'test 
        If-Else
            (if
            ; Operator Call
                (< 
                ; Variable:
                    a 
                ; Variable:
                    b 
                ) ; LtCall
                '(let
                ; Operator Update 
                    '(update 'a 
                        (+ a 
                        ; Number:
                            '1 
                        ) ; OpUpdate-opexpr
                    ) ; OpUpdate
                ; APureExpr: s1 1 2 3 
                ; Service Call
                    '(s1 
                    ; APureExpr: 1 
                    ; Number:
                        '1 
                    ; APureExpr: 2 
                    ; Number:
                        '2 
                    ; APureExpr: 3 
                    ; Number:
                        '3 
                    ) ; FunAppl
                ) ; let
                '(let
                ; APureExpr: s2 3 2 1 
                ; Service Call
                    '(s2 
                    ; APureExpr: 3 
                    ; Number:
                        '3 
                    ; APureExpr: 2 
                    ; Number:
                        '2 
                    ; APureExpr: 1 
                    ; Number:
                        '1 
                    ) ; FunAppl
                ; APureExpr: square 5 
                ; Function Application
                    '(apply square 
                    ; APureExpr: 5 
                    ; Number:
                        '5 
                    ) ; FunAppl
                ) ; let
            ) ; If-Else
        ) ; Assign
    ) ; ParLet
) ; End of program
