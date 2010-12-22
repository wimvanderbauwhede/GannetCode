; SystemConfiguration: DynSobel.SBA.yml 

;        1: [ds_RGB2YCbCr]
;        2: [ds_YCbCr2RGB]
;        3: [ds_MADD]
;        4: [ds_SOBEL_HOR]
;        5: [ds_SOBEL_VERT]

(let
    ; Configure addition
    ;'(s3.reconf '3 (config '3 '1))
    ; Img in
    '(buf 'b0 (img '8))
    ; RGB2YCbCr
    '(buf 'b1 
        (s1.confrun '1 '(config '1 '1)
                    (stream 'b0)
                )
    ) 
    ; Sobel Hor
    '(buf 'b2
        (s2.confrun '4 '(config '4 '1)
            (stream 'b1) '8
            )
    )    
    ; Sobel Vert
    '(buf 'b3
        (s2.confrun '5 '(config '5 '1)
            (get 'b1) '8
            )
    )      
    ; Add
    '(buf 'b4
        (s3.confrun '3 '(config '3 '1)
             (stream 'b2) (stream 'b3) 
             )
    )       
	; YCbCr2RGB
    '(buf 'b5
        (s1.confrun '2 '(config '2 '1)
                    (stream 'b4)
                )
    )    
    ; Img out
   '(label L (iftc (eos 'b5)
        '1
      '(returntc 'L  
       (procimg (stream 'b5))
       )
    ))    
    '(procimg.dump (img.size))
)
