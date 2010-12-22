; SystemConfiguration: DynSobel.SBA.yml 

;        1: [ds_RGB2YCbCr]
;        2: [ds_YCbCr2RGB]
;        3: [ds_MADD]
;        4: [ds_SOBEL_HOR]
;        5: [ds_SOBEL_VERT]

(let
    ; RGB2YCbCr
    '(buf 'b1 (s1.run 
                    (img '8)
                    (s1.reconf (config '1 '1))
                )
    )
    ; Sobel Hor
    '(buf 'b1h (s1.run (get 'b1) '8
                    (s1.reconf (config '4 '1))
                    )
      )
    ; Sobel Vert
    '(buf 'b1v (s1.run (get 'b1) '8
                    (s1.reconf (config '5 '1))
                    )
      )
      
    ; MADD
    '(buf 'b2 (s1.run                 
                    (get 'b1h)
                    (get 'b1v)
                    (s1.reconf (config '3 '1))
            )
    )    
                
    ; YCbCr2RGB
    '(buf 'b3 (s1.run                 
                    (get 'b2)
                    (s1.reconf (config '2 '1))
              )
    )   
    ; out
    '(procimg (stream 'b3))
    '(procimg.dump (img.size))

)
