; Gannet version of Conway's game of life

; 16 cores: b00, b01, b02, b03 etc. In Gannet-C we obviously want b[i][j], and use Gen<>
; we go clockwise and collect rows, cols & corners
; each service receives the image blocks and requests the boundary values from its neighbours.
; somehow a call to e.g. b01.lcol should only return when b01 has received its image block

; what about I/O? One way would be to have a draw service that would draw the returned result of the calculation:
; (draw (b00.calc ...) '0 '0 '0xFF0000)
; This service would draw the frame onto a canvas. Would work well in SystemC
; On the MicroBlaze, it would write the values to the screen.
; I guess I can make it slightly more generic by adding a block dimension

; A very useful addition to Gannet would be (label N '16), i.e. support labels for constants.


; 75x100 pixels would let us do 300x400, or 600x800 if we blow up the image by a factor of 2
; Life needs only bit info, so 7500 pixels is 235 words; a power of 2 would make it a lot easier
; For example, for 1024x768, we need 128x96 pixels, so 384 Words. If we make packets variable between 4 and 8 Words,
; the overhead would be only 1/8. Good. Of course, working on the byteword is expensive, but that's just what we want!

; As we have everyting in software, and no threads, we want one of those fancy self-restarting tasks
; That is a task that does not quite finish: instead, it changes its status to "ready to run" so on the next iteration of the loop it will be put in the queue. Like the "READ" call.

(begin
 ; get the initial values. We don't really need the coordinates here, we can just generate random data
 ; but I guess I'd like nice pictures
'(begin
	(b00.init (img '0 '0) )
	(b01.init (img '0 '1) )
	(b02.init (img '0 '1) )
	(b03.init (img '0 '3) )

	(b10.init (img '1 '0) )
	(b11.init (img '1 '1) )
	(b12.init (img '1 '2) )
	(b13.init (img '1 '3) )

	(b20.init (img '2 '0) )
	(b21.init (img '2 '1) )
	(b22.init (img '2 '2) )
	(b23.init (img '2 '3) )

	(b30.init (img '3 '0) )
	(b31.init (img '3 '1) )
	(b32.init (img '3 '2) )
	(b33.init (img '3 '3) )
)
; calculate, loop forever
; later, detect that all is quiet and rerun the outer loop forever
(label 'L 
	'(begin
		(draw '0 '0
		(b00.calc  (b33.brc) (b30.brow) (b13.blc) (b01.lcol) (b11.tlc) (b10.trow) (b13.trc) (b03.rcol) ))
		(draw '0 '1
		(b01.calc  (b30.brc) (b31.brow) (b32.blc) (b02.lcol) (b12.tlc) (b11.trow) (b10.trc) (b00.rcol) ))
		(draw '0 '2
		(b02.calc  (b31.brc) (b32.brow) (b33.blc) (b03.lcol) (b13.tlc) (b12.trow) (b11.trc) (b01.rcol) ))
		(draw '0 '3
		(b03.calc  (b32.brc) (b33.brow) (b30.blc) (b00.lcol) (b10.tlc) (b13.trow) (b12.trc) (b02.lcol) ))
		
		(draw '1 '0
		(b10.calc  (b03.brc) (b00.brow) (b01.blc) (b11.lcol) (b21.tlc) (b20.trow) (b23.trc) (b13.rcol) ))
		(draw '1 '1
		(b11.calc  (b00.brc) (b01.brow) (b02.blc) (b12.lcol) (b22.tlc) (b21.trow) (b20.trc) (b10.rcol) ))
		(draw '1 '2
		(b12.calc  (b01.brc) (b02.brow) (b03.blc) (b13.lcol) (b23.tlc) (b22.trow) (b21.trc) (b11.rcol) ))
		(draw '1 '3
		(b13.calc  (b02.brc) (b03.brow) (b00.blc) (b10.lcol) (b20.tlc) (b23.trow) (b22.trc) (b12.rcol) ))

		(draw '2 '0
		(b20.calc  (b13.brc) (b10.brow) (b11.blc) (b21.lcol) (b31.tlc) (b30.trow) (b33.trc) (b23.rcol)  ))
		(draw '2 '1
		(b21.calc  (b10.brc) (b11.brow) (b12.blc) (b22.lcol) (b32.tlc) (b31.trow) (b30.trc) (b20.rcol) ))
		(draw '2 '1
		(b22.calc  (b11.brc) (b12.brow) (b13.blc) (b23.lcol) (b33.tlc) (b32.trow) (b31.trc) (b21.rcol) ))
		(draw '2 '3
		(b23.calc  (b12.brc) (b13.brow) (b10.blc) (b20.lcol) (b30.tlc) (b33.trow) (b32.trc) (b22.rcol) ))

		(draw '3 '0
		(b30.calc  (b23.brc) (b20.brow) (b21.blc) (b31.lcol) (b01.tlc) (b00.trow) (b03.trc) (b33.rcol) ))
		(draw '3 '1
		(b31.calc  (b20.brc) (b21.brow) (b22.blc) (b32.lcol) (b02.tlc) (b01.trow) (b00.trc) (b30.rcol) ))
		(draw '3 '2
		(b32.calc  (b21.brc) (b22.brow) (b23.blc) (b33.lcol) (b03.tlc) (b02.trow) (b01.trc) (b31.rcol) ))
		(draw '3 '3
		(b33.calc  (b22.brc) (b23.brow) (b20.blc) (b30.lcol) (b00.tlc) (b03.trow) (b02.trc) (b32.rcol) ))

		'(return L)
	)
)

; I am not clear about the following:
; - difference between (label L ...) and (label 'L ...) -> It must be the first, the 2nd is wrong
; - difference between '(label L (let ...)) and (label L '(let ... )) -> again, the 1st
; - difference between (return 'L) and (return L) -> the first defers to the core, the 2nd gets called from the core, so slower