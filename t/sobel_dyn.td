; SystemConfiguration: DynSobel.SBA.yml 
(let
    ; RGB2YCrCb
    '(buf 'br (s1.run (img '8)
        (s1.reconf '1 (config '1 '1)) ))
    '(let
        ; Sobel-Hor
        (buf 'bsh (s2.run (get 'br)
            (s2.reconf '4 (config '4 '2)) ))
        ; Sobel-Vert
        (buf 'bsv (s3.run (get 'br)
        (s3.reconf '5 (config '5 '3)) ))
    )
    ; MADD
    '(buf 'ba1 (s1.run
        (stream 'bsh) (stream 'bsv)
        (s1.reconf '3 (config '3 '1))
    ))
    ; MADD
    '(buf 'ba2 (s2.run (get 'br) (stream 'ba1)
        (s2.reconf '2 (config '3 '2))
    ))
    ; YCrCb2RGB. I don't understand why this requires the original image
    '(label L 
        (returntc 'L
        (s3.run (stream 'br) (stream 'ba2)
         (s3.reconf '2 (config '2 '3))
        )
        )
     )
     ; (buf 'ba3 (s3.run (stream 'br) (stream 'ba2)
     ;    (s3.reconf '2 (config '2 '3))
     ;   ))
     ;'(label L (if (eos 'ba3) 
     ;'(procimg.dump '8 (img.size))
     ;'(returntc 'L (procimg (stream 'ba3)))
     ;))     
)
