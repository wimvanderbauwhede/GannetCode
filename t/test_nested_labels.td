(let
    (assign 'i '1)

    (label OUTER (if (< (read 'i) '5)
        '(let
        (let
            (assign 'j '1)
            (label INNER (if (< (read 'j) '5)
                '(let
                    (display (read 'j))
                    (update 'j (+ (read 'j) '1))
                    '(return 'INNER)
                )
                '(return '1)
            ))
            (return '3)
            )
            (display (* '10 (read 'i)))
            (update 'i (+ (read 'i) '1))
            '(return 'OUTER)
        )
        '(return '2)
        ))
    (return '4)
)
