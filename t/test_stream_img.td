; SystemConfiguration: DynSobel.SBA.yml 

;        1: [ds_RGB2YCbCr]
;        2: [ds_YCbCr2RGB]
;        3: [ds_MADD]
;        4: [ds_SOBEL_HOR]
;        5: [ds_SOBEL_VERT]

(let
    ; Img in
    '(buf 'b0 (img '8))
    
    ; Img out
   '(label L (iftc (eos 'b0)
        '1
      '(returntc 'L  
       (procimg (stream 'b0))
       )
    ))    
    '(procimg.dump (img.size))
)
