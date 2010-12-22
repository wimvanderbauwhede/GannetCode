; SystemConfiguration: SysCSobelDynRefNoStream.SBA.yml

; full Sobel for System-C. Dynamic services but no actual dyn reconf, as ref
;        1: [dyn_rgb2ycbcr.rb, ds_RGB2YCbCr]
;        2: [dyn_ycbcr2rgb.rb, ds_YCbCr2RGB]
;        3: [dyn_madd.rb, ds_MADD]
;        4: [dyn_sobel.rb, ds_SOBEL_HOR]
;        5: [dyn_sobel.rb, ds_SOBEL_VERT]
;        6: [dyn_mirror.rb, ds_MIRROR]
(let
    ; '(buf 'b0 (img '16) )
    ; we reconfigure all services in parallel
    '(let
    (buf 'b0 (img '16) )
    (r2y.reconf '1 (config '1 '1)) 
    (mir1.reconf '6 (config '6 '1)) 
    (mir2.reconf '6 (config '6 '1)) 
    (sobh1.reconf '4 (config '4 '1)) 
    (sobv1.reconf '5 (config '5 '1)) 
    (sobh2.reconf '4 (config '4 '1)) 
    (sobv2.reconf '5 (config '5 '1)) 
    (madd1.reconf '3 (config '3 '1)) 
    (madd2.reconf '3 (config '3 '1)) 
    (madd3.reconf '3 (config '3 '1)) 
    (y2r.reconf '2 (config '2 '1)) 
    )
    '(let
 	(buf 'b1 (r2y.run (get 'b0) ))
 	(buf 'bm1 (mir1.run (get 'b1) ))
	(buf 'bsh1 (sobv1.run (get 'b1) '16))
	(buf 'bsv1 (sobh1.run (get 'b1) '16))
	(buf 'bsh2 (sobv2.run (get 'bm1) '16))
	(buf 'bsv2 (sobh2.run (get 'bm1) '16))
	(buf 'ba1 (madd1.run (get 'bsh1) (get 'bsv1) ))
	(buf 'ba2 (madd2.run (get 'bsh2) (get 'bsv2) ))
	(buf 'bm2 (mir2.run (get 'bm1) ))
	(buf 'ba3 (madd3.run (get 'ba1) (get 'bm2) ))
	(buf 'b2 (y2r.run (get 'ba3) ))	
	(procimg (get 'b2))
	)
	'(procimg.dump (img.size)) 
)
