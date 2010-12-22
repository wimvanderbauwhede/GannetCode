; This is an example of the use of 'cache' vars as mutable variables,
; with the main purpose of supporting accumulation.
; Although technically there is no difference at all between 'cache' and 'var/acc',
; in practice there are three key diffs: 
; 1. var/acc is mutable (maybe I'll use var/set ?? or var/update? Would be OK if let-vars are immutable
; 2. the return value of 'cache' is used by the service, it must be the actual data
; the return value of 'var/acc' is rather irrelevant
; 3. var/acc really only works in a sequential let-style block; but that is too heavy for a service
; so at machine level it should be a 'return'; however, at IR level it is difficult to parse
; a 'return' such that the 'var' assignments are in scope ... 

;(S1-return
; '(label L ( ... ))
; (var 'v1 ...) ; now what if v1 requires v2? 
; (var 'v2 ...)
; )

; So what we really should do is: 
; - first scan the expression for var-bindings, without descending into the expressions
;any deeper than required to parse the (var 'v1 (...)), and then only if it's actually a 'var/acc'
; - then parse the expression in proper order.

(return
; (label L 
;    (if (> (get 'v1) '5)
    '(return (* (get 'acc1) (get 'v1)))
;    '(return L (par 'v1 (+ (get 'v1) '1)))
;    )
; )
 (var 'v1 '1)
 (var 'acc1 '10)
 )
