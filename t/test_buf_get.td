; simple test for buffer support
(let
   '(buf 'b1 (* '4 '7 ) )
   '(+ '2 '3 (get 'b1) '4 '5) ; => 42
)    
