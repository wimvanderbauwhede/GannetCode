; simple test for buffer support
(begin
   (buf 'b1 (* '6 '7 ) )
   (+ '2 '3 (stream 'b1 ) '4 '5 )
)    
