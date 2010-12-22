; SystemConfiguration: DynSobel.SBA.yml 

;        1: [ds_RGB2YCbCr]
;        2: [ds_YCbCr2RGB]
;        3: [ds_MADD]
;        4: [ds_SOBEL_HOR]
;        5: [ds_SOBEL_VERT]

(let
    ; Img in
    '(buf 'b0 (img '8))
    
    ; RGB2YCbCr
    '(buf 'b1 
        (s1.confrun '1 '(config '1 '1)
                    (stream 'b0)
                )
    ) 
      
	; YCbCr2RGB
    '(buf 'b2
;        (madd
        (s1.confrun '2 '(config '2 '1)
                    (stream 'b1)
                )
    )    
    ; Img out
   '(label L (iftc (eos 'b2)
        '1
      '(returntc 'L  
       (procimg (stream 'b2))
       )
    ))    
    '(procimg.dump (img.size))
)
