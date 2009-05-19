(let
	'(fopen '1 "SBA.yml" "r")
	'(fopen '2 "tmpp" "w")
'(label L
    (if
	 (== '1 (iowrite '2 (ioread '1 '0)))
    '(return 'L)
    '(return '1)
)
)
'(fclose '1)
'(fclose '2)
)

