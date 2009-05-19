; simple test for pipeline of matrix operations
(let
   '(buf 'b1 (a))
   '(buf 'b3 (b))
   '(buf 'b2 (tran (stream 'b1)))
   '(buf 'b4 (madd (stream 'b2) (stream 'b3)))   
   '(buf 'b5 (scale '2 (stream 'b4)))
   '(label L 
        (returntc 'L (inv1 (stream 'b5)))
   )
)    
