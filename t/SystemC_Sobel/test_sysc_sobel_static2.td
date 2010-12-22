; SystemConfiguration: SystemC_Sobel/StaticNoStream.SBA.yml

; full Sobel for System-C

(let
 	(buf 'b1 (r2y (img '16) ) )
 	(buf 'bm (mir2 (get 'b1) ) )
	(procimg 
		(y2r
		 	(madd3
			 (madd2 
			  	(sobh2 (get 'b1) '16)
			  	(sobv2 (get 'b1) '16)
			  )
			 (mir2 
			 (madd1 
			  	(sobh1 (get 'bm '16))
			  	(sobv1 (get 'bm '16))
			 )
			 )
		 )
	   )
	 )
	'(procimg.dump (img.size)) 
)
