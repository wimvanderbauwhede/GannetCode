; SystemConfiguration: SystemC_Sobel/StaticNoStream.SBA.yml

; full Sobel for System-C

(let
 	'(buf 'b1 (r2y (img '16) ) )
 	'(buf 'bm (mir2 (get 'b1) ) )
	'(buf 'bsh1 (sobh1 (get 'b1) '16))
	'(buf 'bsv1 (sobv1 (get 'b1) '16))
	'(buf 'bsh2 (sobh2 (get 'bm) '16))
	'(buf 'bsv2 (sobv2 (get 'bm) '16))
	'(buf 'ba1 (madd1 (get 'bsh1) (get 'bsv1)))
	'(buf 'ba2 (madd2 (get 'bsh2) (get 'bsv2)))
	'(buf 'bm2 (mir2 (get 'ba2)))
	'(buf 'ba3 (madd3 (get 'ba1) (get 'bm2)))
	'(buf 'b2 (y2r (get 'ba3)))
	'(procimg (get 'b2))
	'(procimg.dump (img.size)) 
)
