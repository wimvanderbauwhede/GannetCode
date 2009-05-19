; simple test for buffer support
(return
   '(+ '2 '3 (stream 'b1) '4 '5)
   (buf 'b1 (* '6 '7))
)    
