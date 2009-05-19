; Syntax definitions for Skua
(define-syntax (label stx) 
  (syntax-case stx ()
    [(_ L expr ...) (syntax (let ((L #f)) (call/cc (lambda (k) (set! L k))) expr ... ))]))

; not used, we now use call/cc
(define-syntax (return-label-lambda stx) 
  (syntax-case stx ()
    [(_ L expr ...) (syntax (let () expr ... (L L)))]))

(define-syntax (label-lambda stx)
  (syntax-case stx ()
    [(_ L body)
     (begin
       ;; If id is not an identifier, report an error in terms of let1 instead of let:
       (if (not (identifier? (syntax L)))
           (raise-syntax-error #f "expected an identifier" stx (syntax L)))
       (syntax ((lambda (L) (L L)) (lambda (L) body))))]))

(define-syntax (async stx) 
  (syntax-case stx ()
    [(_ expr ...) (syntax (thread-start! (make-thread (lambda () (let () expr ... )))))]))  


; Some nice syntactic sugar 
; (async ((-> S 'm) (list ...))) => (~> S 'm ...)
; Now gannet-to-scheme becomes very readable and very easy to compile
(define-syntax (~> stx) 
  (syntax-case stx ()
    [(_ S m expr ...) (syntax (async ((-> S m) (list expr ... ))))]))  

(define-syntax (method_ stx)
  (syntax-case stx ()
    [ (_ self methodname args expr) (syntax (->! self methodname (lambda (args) expr)))]))

;(->! self METH (lambda (args) (-> self 'lock (IMPL)))) 
;=> (service-method self METH (IMPL)
(define-syntax (service-method stx)
    (syntax-case stx ()
    [(_ self methodname args expr) (syntax (->! self methodname (lambda (args) ((-> self 'lock) expr)) )) ])
)

