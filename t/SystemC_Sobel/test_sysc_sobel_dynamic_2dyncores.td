; SystemConfiguration: SystemC_Sobel/DynNoStream.SBA.yml

; full Sobel for System-C. Dynamic services but no actual dyn reconf, as ref
;        1: [dyn_rgb2ycbcr.rb, ds_RGB2YCbCr]
;        2: [dyn_ycbcr2rgb.rb, ds_YCbCr2RGB]
;        3: [dyn_madd.rb, ds_MADD]
;        4: [dyn_sobel.rb, ds_SOBEL_HOR]
;        5: [dyn_sobel.rb, ds_SOBEL_VERT]
;        6: [dyn_mirror.rb, ds_MIRROR]
(let
    '(let ; run img, config&run r2y; config mirr
		(buf 'b0 (img '16) )
		(buf 'b1 (s1.confrun '1 '(config '1 '1) (get 'b0)))
		(s2.reconf '6 (config '6 '1))
	 )
    '(let ; reconf sobel-h; run mirr
 		(s1.reconf '4 (config '4 '1))	
 		(buf 'bm (s2.run (get 'b1) ))
	)
	'(let ; reconf sobel-v; run sobel-h
		(s2.reconf '5 (config '5 '1))
		(buf 'bsh1 (s1.run (get 'b1) '16))
		(buf 'bsh2 (s1.run (get 'bm) '16))
	 )
	'(let ; reconf add; run sobel-v
 		(s1.reconf '3 (config '3 '1))	
		(buf 'bsv1 (s2.run (get 'b1) '16))
		(buf 'bsv2 (s2.run (get 'bm) '16))
	)
	'(let ; reconf mirr; run add (2x); conf&run mirr
		(buf 'ba1 (s1.run (get 'bsh1) (get 'bsv1) ))
		(buf 'ba2 (s1.run (get 'bsh2) (get 'bsv2) ))
		(buf 'bm2 (s2.confrun '6 '(config '6 '1) (get 'ba2) ))
	)
	'(let ; run add; conf&run y2r; run procimg
		(buf 'ba3 (s1.run (get 'ba1) (get 'bm2) ))
		(buf 'b2 (s2.confrun '2 '(config '2 '1) (get 'ba3) ))	
		(procimg (get 'b2))
	)
	'(procimg.dump (img.size)) 
)
