(let
    (assign 'c '1)
    '(label L 
        (iftc (> c '324) ; FAILS at 255 due to subtask stack overflow in APPLY. 
        '(return c)
        '(returntc 'L 
        (update 'c (+ c '1))
            (let
            	    (assign 'f (lambda 'n 'acc 'f_ '(if (< n '2) 
						'(return acc)
						'(applytc (read f_) (- n '1) (* acc n) f_)
		    			)
        		      )
           	       )
    	        '(apply f '12 '1 'f) ;
                '(return '1)
            )
            )
    )
    )
)   
