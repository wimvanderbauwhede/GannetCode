; simple test for buffer support
(let
   '(buf 'b1 (* '6 '2))
   '(buf 'b2 (+ '1 '3 (stream 'b1) '4 '5))
   '(+ '4 (stream 'b2) '9)
   '(+ '8 (stream 'b2) '9)
)    
