; SystemConfiguration: DynSobel.SBA.yml

; full Sobel for System-C. Dynamic services but no actual dyn reconf, as ref
;        1: [dyn_rgb2ycbcr.rb, ds_RGB2YCbCr]
;        2: [dyn_ycbcr2rgb.rb, ds_YCbCr2RGB]
;        3: [dyn_madd.rb, ds_MADD]
;        4: [dyn_sobel.rb, ds_SOBEL_HOR]
;        5: [dyn_sobel.rb, ds_SOBEL_VERT]
;        6: [dyn_mirror.rb, ds_MIRROR]
(let
    '(buf 'b0 (img '16) )
 	'(buf 'b1 (s1.confrun '1 '(config '1 '1) (get 'b0) ))
 	'(buf 'b2 (s1.confrun '6 '(config '6 '1) (get 'b1) ))
	'(buf 'b3 (s1.confrun '4 '(config '4 '1) (get 'b1) '16))
	'(s1.confrun '4 '(config '4 '1) (get 'b1) '16)
	'(s1.confrun '5 '(config '5 '1) (get 'b1) '16)
	'(s1.confrun '5 '(config '5 '1) (get 'b1) '16)
	'(buf 'b4 (s1.confrun '3 '(config '3 '1) (get 'b3) (get 'b1) ))
	'(buf 'b5 (s1.confrun '3 '(config '3 '1) (get 'b3) (get 'b4) ))
	'(s1.confrun '6 '(config '6 '1) (get 'b5) )	
	'(buf 'b6 (s1.confrun '2 '(config '2 '1) (get 'b5) ))	
	'(procimg (get 'b6))
	'(procimg.dump (img.size)) 
)
