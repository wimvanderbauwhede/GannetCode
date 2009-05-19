; simple test for pipeline of matrix operations
(let
   '(buf 'b1 (a))
   '(buf 'b2 (tran (stream 'b1)))
   '(label L 
        (returntc 'L (scale '2 (stream 'b2)))
   )
)    
