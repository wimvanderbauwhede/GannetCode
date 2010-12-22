; SystemConfiguration: DynSobel.SBA.yml

; full Sobel for System-C. Dynamic services but no actual dyn reconf, as ref
;        1: [dyn_rgb2ycbcr.rb, ds_RGB2YCbCr]
;        2: [dyn_ycbcr2rgb.rb, ds_YCbCr2RGB]
;        3: [dyn_madd.rb, ds_MADD]
;        4: [dyn_sobel.rb, ds_SOBEL_HOR]
;        5: [dyn_sobel.rb, ds_SOBEL_VERT]
;        6: [dyn_mirror.rb, ds_MIRROR]
(let
    '(let 
		(buf 'b0 (img '16) )
		(s1.reconf '1 (config '1 '1))
		(s2.reconf '4 (config '4 '1))
		(s3.reconf '5 (config '5 '1))
	 )
    '(let
 		(buf 'b1 (s1.run (get 'b0) ))
 		'(buf 'bm (s1.confrun '6 '(config '6 '1) (get 'b1) ))
	)
	'(let
		(s1.reconf '3 (config '3 '1))
		(buf 'bsh1 (s3.run (get 'b1) '16))
		(buf 'bsv1 (s2.run (get 'b1) '16))
		(buf 'bsh2 (s3.run (get 'bm) '16))
		(buf 'bsv2 (s1.run (get 'bm) '16))
	)
	'(let
		(buf 'ba1 (s1.confrun '3 '(config '3 '1) (get 'bsh1) (get 'bsv1) ))
		(buf 'ba2 (s1.confrun '3 '(config '3 '1) (get 'bsh2) (get 'bsv2) ))
		(s2.reconf '6 (config '6 '1)  )
		(s3.reconf '2 (config '2 '1)  )
	)
	'(let
		(buf 'bm2 (s2.run (get 'ba2) ))
		(buf 'ba3 (s1.run (get 'ba1) (get 'bm2) ))
		(buf 'b2 (s3.run (get 'ba3) ))	
	)
	'(procimg (get 'b2))
	'(procimg.dump (img.size)) 
)
