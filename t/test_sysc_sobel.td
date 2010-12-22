; SystemConfiguration: SysCSobel.SBA.yml 

(let
    ; Img in
    '(buf 'b0 (img '8))
    ; RGB2YCbCr
    '(buf 'b1 (r2y (stream 'b0)))
    ; Sobel-Vert
;    '(buf 'bsv (sobv (stream 'b1)))
    ; Sobel-Hor
;    '(buf 'bsh (sobh (stream 'b1)))    
    ; Add
;    '(buf 'bs1 (madd (stream 'bsv) (stream 'bsh)))
    ; Add
    '(buf 'bs2 (madd (stream 'b1)) ) ;(stream 'bs1)))
    ; YCbCr2RGB
    '(buf 'b2 (y2r (stream 'bs2)))    
    ; Img out
   '(label L (iftc (eos 'b2)
        '(procimg.dump (img.size))
      '(returntc 'L  
       (procimg (stream 'b2))
       )
    ))    
   ; '(procimg.dump (img.size))
)
