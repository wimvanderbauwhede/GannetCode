; this compiles OK but and should run OK
; This example illustrates the use of (cache 'c1 ...),
; it really is an explicit cacheing mechanism, the cached value is read-only
; And I don't care that it only works if the cached call is first in lexical order
; i.e. (S1 (get 'c1) (cache 'c1 (S3 ))) does not work
(S1
     (cache 'c1 (S3 ))
     (S2 (get 'c1) )
     (get 'c1)
)

