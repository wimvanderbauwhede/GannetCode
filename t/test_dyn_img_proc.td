; SystemConfiguration: DynSobel.SBA.yml 

;        1: [ds_RGB2YCbCr]
;        2: [ds_YCbCr2RGB]
;        3: [ds_MADD]
;        4: [ds_SOBEL_HOR]
;        5: [ds_SOBEL_VERT]

(let
    ; RGB2YCbCr
    '(buf 'b1 (s1.confrun '(config '1 '1)
                    (img '8)
                )
    )
    ; Sobel Hor
    '(buf 'b1h (s1.confrun 
                    '(config '4 '1)
                     (get 'b1) '8)
      )
    ; Sobel Vert
    '(buf 'b1v (s1.confrun 
                    '(config '5 '1)
                     (get 'b1) '8)
      )
      
    ; MADD
    '(buf 'b2 (s1.confrun          
                   '(config '3 '1)       
                    (get 'b1h)
                    (get 'b1v)
            )
    )    
                
    ; YCbCr2RGB
    '(buf 'b3 (s1.confrun          
                   '(config '2 '1)       
                    (get 'b2)
              )
    )   
    ; out
    '(procimg (stream 'b3))
    '(procimg.dump (img.size))

)
