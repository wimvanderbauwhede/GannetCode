; SystemConfiguration: GannetC.SBA.yml
(let
	'(io.open '1 "SBA.yml" "r")
	'(io.open '2 "tmpp" "w")
'(label L
    (if
	 (== '1 (io.write '2 (io.read '1 )))
    '(return 'L)
    '(return '1)
)
)
'(io.close '1)
'(io.close '2)
)

