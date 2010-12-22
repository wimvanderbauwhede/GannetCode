; SystemConfiguration: DynConfig.SBA.yml
(return 
'(s1run '11 
(return
'(s2run '2 '3) (s2reconf (config '2 '1) )
)
(return
 '(s3run '3 '4)
    (s3reconf (config '3 '1) ) 
 )
) 
    (s1reconf (config '1 '1) ) 
)
