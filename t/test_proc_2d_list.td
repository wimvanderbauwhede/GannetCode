(let
(assign 'img
    (list
                (list '139 '144 '149 '153 '155 '155 '155 '155)
                (list '144 '151 '153 '156 '159 '156 '156 '156)
                (list '150 '155 '160 '163 '158 '156 '156 '156)
                (list '159 '161 '162 '160 '160 '159 '159 '159)
                (list '159 '160 '161 '162 '162 '155 '155 '155)
                (list '161 '161 '161 '161 '160 '157 '157 '157)
                (list '162 '162 '161 '163 '162 '157 '157 '157)
                (list '162 '162 '161 '161 '163 '158 '158 '158)
                )
			 )
(let
(assign 'img1 (read 'img)) ; first row
(assign 'runs '0)
'(label R (if (< (read 'runs) '8)
    '(let
     (update 'runs (+ '1 (read 'runs))) 
    (assign 'i 0)
    (assign 'img2 (read (head 'img1))) ; copy of first row
    '(update 'img1 (tail (read 'img1)))
    (label I (if (< (read 'i) '8)
    '(let
        (update 'i (+ (read 'i) '1))
        (assign 'elt (head (read 'img2)))
        '(update 'img2 (tail (read 'img2))) ; remove head
        (display (read 'elt))
        '(return 'I)
    )
    ;'(return 'I)
    '(return '1)
    ))
     '(return 'R)
     )
    '(read 'runs)
))
)
)
