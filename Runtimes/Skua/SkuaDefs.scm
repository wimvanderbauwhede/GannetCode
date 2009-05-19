; Definitions for Skua
; Gambit doesn't have fold 
(define (fold f ls) (let
                        (
                         (acc (car ls))
                         (rls (cdr ls))
                         )
                      (define (fold-acc f ls acc)
                        (if (> (length ls) 0)
                            (if (= (length ls) 1)
                                (let (
                                      (x (car ls))
                                      )
                                  (fold-acc f (list) (f acc x))
                                  )
                                (let (
                                      (x  (car ls))
                                      (xs (cdr ls))
                                      )
                                  (fold-acc f xs (f acc x)))
                                )
                            acc
                            )
                        )
                      (fold-acc f rls acc)
                      ))

; Scheme is not strong on currying, but still
(define (uncurry-rec f ls) (let
                        (
                         (fc (f (car ls)))
                         (rls (cdr ls))
                         )
                        (if (> (length rls) 0)
                           (uncurry-rec fc rls)
                           fc
                            )
                      ))
(define (uncurry f ls) (uncurry-rec (lambda (z) (f z)) ls))
;usage:
;(define curried-lambda (lambda (x) (lambda (y) (lambda (z) (* x y z) ))) )
;(uncurry curried-lambda '(7 6 5))

;Some shorter aliases for hash table functions
(define (-> inst meth) (table-ref inst meth))
(define (->! inst meth val) (table-set! inst meth val))
(define (->del! inst meth) (table-set! inst meth))
(define (->has? inst meth) (let ((p (table-ref inst meth #f))) (if p #t #f)) )
(define (new) (make-table))  

;In case we need to wait
(define (wait-for ht k) (if (table-ref ht k #f) #t (wait-for ht k)))

