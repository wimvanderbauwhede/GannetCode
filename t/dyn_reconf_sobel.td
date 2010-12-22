; SystemConfiguration: DynMatrixMORA.SBA.yml 
(let
    ; RGB2YCrCb
    '(buf 'br (s1run (rand)
        (s1reconf (config '1 '1)) ))
    '(let
        ; Sobel-Hor
        (buf 'bsh (s2run (get 'br)
            (s2reconf (config '4 '2)) ))
        ; Sobel-Vert
        (buf 'bsv (s3run (get 'br)
        (s3reconf (config '5 '3)) ))
    )
    ; MADD
    '(buf 'ba1 (s1run
        (stream 'bsh) (stream 'bsv)
        (s1reconf (config '3 '1))
    ))
    ; MADD
    '(buf 'ba2 (s2run (get 'br) (stream 'ba1)
        (s2reconf (config '3 '2))
    ))
    ; YCrCb2RGB
    '(label L 
        (return 'L
        (s3run (stream 'br) (stream 'ba2)
         (s3reconf (config '2 '3))
        )
        )
     )
)
