; SystemConfiguration: ImgOO.SBA.yml 

(let
    ; Img in
    '(buf 'b0 (img.in '8))
    
    ; Img out
   '(label L (iftc (eos 'b0)
        '1
      '(returntc 'L  
       (procimg.in (stream 'b0))
       )
    ))    
    '(procimg.dump (img.size))
)
