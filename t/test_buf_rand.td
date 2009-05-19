; simple test for buffer support
(let
   '(buf 'b1 (rand '0 '10))
   '(buf 'b2 (+ '100 (stream 'b1)))
   '(label L 
        (returntc 'L (* '4 (stream 'b2)))
   )
)    
