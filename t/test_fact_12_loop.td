(let
    (assign 'c '1)
    '(label L 
        (if (> c '1) ; FAILS at 127 due to subtask stack overflow. 
        '(return c)
        '(return 'L 
        (update 'c (+ c '1))
            (let
            	    (assign 'f (lambda 'n 'acc 'f_ '(if (< n '2) 
						'(return acc)
						'(applytc (read f_) (- n '1) (* acc n) f_)
		    			)
        		      )
           	       )
    	        '(apply f '9 '1 'f) ; FAILS at 9!  if c > 2
                '(return '1)
            )
            )
    )
    )
)   
