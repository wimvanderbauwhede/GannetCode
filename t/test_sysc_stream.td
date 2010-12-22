; SystemConfiguration: SysCSobel.SBA.yml 

(let
    ; Img in
    '(buf 'b0 (img '8))
    ; RGB2YCbCr
    ;'(buf 'b1 (r2y (stream 'b0)))
    ; Add
    '(buf 'b1 (madd (stream 'b0)) )
    ; YCbCr2RGB
    ;'(buf 'b2 (y2r (stream 'b0)))    
    ; Img out
   '(label L (iftc (eos 'b1)
        '(procimg.dump (img.size))
      '(returntc 'L  
       (procimg (stream 'b1))
       )
    ))    
   ; '(procimg.dump (img.size))
)
