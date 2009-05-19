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
			 
(assign 'nrows (length (read 'img)) )
(assign 'show (lambda 'ws '(display ws)))

(let
    (assign 'runs '0)

        (assign 'elt  (head (head (read 'img))))
        (apply (read 'show) (read 'elt))

(label R (if (< (read 'runs) (- (read 'nrows) '1))
    '(let
     (update 'runs (+ '1 (read 'runs))) 
     (let
        (assign 'elt  (head (head (tail (read 'img)))))
        (apply (read 'show) (read 'elt))
        '(update 'img (tail (read 'img))) 
     )
     '(return 'R)
     )
    '(return '1)
))
)
)
