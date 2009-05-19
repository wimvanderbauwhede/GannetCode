; Skua -  a Scheme runtime library for Gannet
; To use this:
; * compile a Gannet program with hags -S
; * create a wrapper which loads, in this order:
;    - SkuaSyntax.scm
;    - SkuaDefs.scm
;    - Skua.scm
;    - yourscript.scm

;Gateway

;(define (ev arg) (if (thread? arg)
;                              (thread-join! arg)
;                                     (force arg)
;                                            )
;)
(define (rec-evl arg) (if (thread? arg) 
                                                   (let ((r (thread-join! arg)))
                                                     (if (thread? r)
                                                         (rec-evl r)
                                                         r
                                                         ))                                                  
                                                   arg))

(define (gw arg) 
  (display
   ;(if (thread? arg)
   ;    (thread-join! arg)
   ;    arg
   ;    )
   (rec-evl arg)
   )
  (newline)
  )

;Base object definition
(define (Service) (let
                      ((self (new)) )                      
                    (->! self 'unq (lambda (x) (force x) ))
                    (->! self 'evl (lambda (x) (if (thread? x) 
                                                   (let ((r (thread-join! x)))
                                                     (if (thread? r)
                                                         ((-> self 'evl) r)
                                                         r
                                                         ))                                                  
                                                   x)))
                    (->! self 'ev (lambda (x) (map (-> self 'evl) x)))
                    ; create a semaphore
                    (->! self 'mutex (make-mutex))
;                    (->! self 'count 0)
                    ; create a wrapper to lock method calls
; in Gannet IP cores are assumed to be single-threaded, so only one thread per tile can be active
; in fact the model is still not completely realistic as all control services (including the ALU)
; are actually implemented by a single service, the GannetVM. 
; However, the GannetVM emulates parallelism.                    
                    (->! self 'lock (lambda (meth) (let ((res 'nil)) 
                        (mutex-lock! (-> self 'mutex)) 
; next lines are to demonstrate that the lock does work                        
 ;                       (->! self 'count (+ (-> self 'count) 1))
 ;                       (thread-sleep! 1) (display (-> self 'count)) (newline)
                        (set! res meth) 
                        (mutex-unlock! (-> self 'mutex))
                        res
                        )))
                    self
                    )
  )

; locking: the idea is that e.g. only one ALU thread can be active at once.
; So when we try to run a thread, it is either locked or we lock it, then we run
; So suppose we call (+ ...), then we first lock the mutex, then do the +, then unlock it


; Generic 
(define (GEN)
  (let (
        (self (Service))
        )
    self
    )
  )

;ALU 
(define (ALU)
  (let (
        (self (Service))
        )
;; See SkuaSyntax: service-method is sugar for a method with a lock        
    (service-method self 'plus args
                      (fold + ((-> self 'ev) args) ))
    (service-method self 'minus args
                       (fold - ((-> self 'ev) args) )) 
    (service-method self 'times args
                       (fold * ((-> self 'ev) args) )) 
    (service-method self 'over args
                      (fold / ((-> self 'ev) args) )) 
    (service-method self 'lt args  ;(display "lt\n")
                    (< (car ((-> self 'ev) args)) (cadr ((-> self 'ev) args)) )) 
    (service-method self 'gt args ;(display "gt\n")
                    (> (car ((-> self 'ev) args)) (cadr ((-> self 'ev) args)) )) 
    (service-method self 'eq args
                    (= (car ((-> self 'ev) args)) (cadr ((-> self 'ev) args)) ))      
    (service-method self 'rnd args (random-real))

    
;    (->! self 'plus (lambda (args) ;(display "plus\n")
;                      (fold + ((-> self 'ev) args) )))                  
;    (->! self 'minus (lambda (args) ;(display "minus\n")
;                       (fold - ((-> self 'ev) args) ))) 
;    (->! self 'times (lambda (args) ;(display "times\n")
;                       (fold * ((-> self 'ev) args) ))) 
;    (->! self 'over (lambda (args)
;                      (fold / ((-> self 'ev) args) ))) 
;    (->! self 'lt (lambda (args)  ;(display "lt\n")
;                    (< (car ((-> self 'ev) args)) (cadr ((-> self 'ev) args)) ))) 
;    (->! self 'gt (lambda (args) ;(display "gt\n")
;                    (> (car ((-> self 'ev) args)) (cadr ((-> self 'ev) args)) ))) 
;    (->! self 'eq (lambda (args)
;                    (= (car ((-> self 'ev) args)) (cadr ((-> self 'ev) args)) )))      
;    (->! self 'rnd (lambda (args) (random-real)))
    self
    )
  )
;Cond
(define (Cond)
  (let (
        (self (Service))
        )
    (service-method self 'if args
                    (let* ( (evargs ((-> self 'ev) args))
                            (c (car evargs))
                            (t (cadr evargs))
                            (f (caddr evargs))
                            )              
                      (if c
                          ((-> self 'evl) ((-> self 'unq) t))
                          ((-> self 'evl) ((-> self 'unq) f))
                          )
                      ))
    (service-method self 'return args
                        (let* ((evargs ((-> self 'ev) args))
                               (arg (car evargs))
                               )  
                          ((-> self 'evl) ((-> self 'unq) arg))
                          ))      
    self
    )
  )
;Block
(define (Block)
  (let (
        (self (Service))
        )
    (->! self 'store (new))
    (service-method self 'begin args (car (reverse  ((-> self 'ev) args))))
    (service-method self 'let args (let* (
                                         (evargs ((-> self 'ev) args))
                                         (unqevargs (map (lambda (arg) ((-> self 'evl) ((-> self 'unq) arg))) evargs))
                                         )
                                    
                                    (map (lambda (k) (->del! (-> self 'store) k)) unqevargs)
                                  ; (display "exit LET:")
                                  ; (display (car (reverse unqevargs)))
                                  ; (newline)
                                    (car (reverse unqevargs))                   
                                    ))
    (service-method self 'assign args 
                        (let* (
                               (evargs ((-> self 'ev) args))
                               (var (car evargs))
                               (val (cadr evargs))
                               )
                          
                          ;                         (display "ASSIGN\n")
                          ;                         (thread-sleep! 5)
                          (->! (-> self 'store) var val)
                          ;                         (display "ASSIGN done\n")
                          var
                          )
                        )
    (service-method self 'read args 
                      (let* ((evargs ((-> self 'ev) args))
                             (var (car evargs)))
                        ;                       (display "READ\n")
                        (wait-for (-> self 'store) var)
                        ;                       (display "READ done\n")
                        (-> (-> self 'store) var)                                        
                        )
                      )
    (service-method self 'update args                                         
                        (let* ((evargs ((-> self 'ev) args))
                               (var (car evargs))
                               (val (cadr evargs))
                               )
                                                  ;  (display "UPDATE-wait\n") (display var) (display val) (newline)
                          (wait-for (-> self 'store) var)
                          (->! (-> self 'store) var val)
;                          (display "UPDATE-done\n")
                          val
                          )
                        )
    (service-method self 'list args ((-> self 'ev) args))
    (service-method self 'head args (caar ((-> self 'ev) args)))
    (service-method self 'tail args (cdar ((-> self 'ev) args)))
    (service-method self 'length args (length (car ((-> self 'ev) args))))
    (service-method self 'cons args (let* ((evargs ((-> self 'ev) args))
                                          (lst (car evargs))
                                          (elt (cadr evargs))
                                          )
                                     (append lst (list elt))
                                     ))
    (service-method self 'append args (let* ((evargs ((-> self 'ev) args))
                                          (lst1 (car evargs))
                                          (lst2 (cadr evargs))
                                          )
                                     (append lst1 lst2)
                                     ))
    self
    )
  )

; Function 
(define (Function)
  (let (
        (self (Service))
        )
    (service-method self 'lambda args (car args))
    (service-method self 'apply args (let* ((evargs ((-> self 'ev) args))
                                           (lambda-body (car evargs))
                                           (lambda-args (cdr evargs))
                                           )
                                      ;                                      (map (lambda (x) (display x) (newline)) evargs)
                                      ;((-> self 'evl) (uncurry lambda-body lambda-args))
                                      ((-> self 'evl) ((-> self 'unq) (uncurry lambda-body lambda-args)))
                                      ))
    (->! self 'applyrec (-> self 'apply))
    self
    )
  )


; Fifos
; We assume that there is an individial FIFO per service, so we should actually instantiate different FIFOs
; A FIFO service should actually never get parallel requests, so I guess I don't need locking here.
; Without locks a single FIFO instance can serve all FIFOs in the program
(define (Buffer)
  (let (
        (self (Service))
        )
    (->! self 'store (new))
    (->! self 'refs (new))
    ;my $l=@args[1];
    ;@.store[@args[0]].push(@$l);
    (->! self 'fifo (lambda (args) (let* (
                                          (evargs ((-> self 'ev) args))
                                          (k (car evargs))
                                          (ls (cdr evargs))
                                          )
                                     (if (->has? (-> self 'store) k)
                                         (->! (-> self 'store) k (append (-> (-> self 'store) k) ls))
                                         (->! (-> self 'store) k ls)
                                         )
                                     )))
    ;my $r=@.store[@args[0]].shift();
    (->! self 'get (lambda (args) (let* (
                                         (evargs ((-> self 'ev) args))
                                         (k (car evargs))
                                         )
                                    ; check here if there is anything in the store!
                                    ; if not, wait until there is something!!!
                                    ; otherwise, return it and cdr the store
                                    (wait-for (-> self 'store) k)
                                    (let* (
                                           (ls (-> (-> self 'store) k))
                                           (r (car ls))
                                           )
                                      (->! (-> self 'store) k (cdr ls))                                          
                                      r
                                      )
                                    )))
    
    ; (buffer sid 'arg)
    ; buffer pushes onto a list, so we can init
    ; the pipe with multiple values
    (->! self 'buffer (lambda (args) (let* (
                                            (evargs ((-> self 'ev) args))
                                            (i (car evargs))
                                            (s (cadr evargs)) 
                                            (r ((-> self 'evl) ((-> self 'unq) s)))                                            
                                            )                                       
                                       (->! (-> self 'refs) i s)
                                       (if (->has? (-> self 'store) i)
                                           (->! (-> self 'store) i (append (-> (-> self 'store) i) (list r)))
                                           (->! (-> self 'store) i (list r))
                                           )                                       
                                       ;%.refs{$i}=$s;
                                       ;@.store[$i].push($r);    
                                       r ; actually no need to return anything
                                       )))
    ; stream returns the store and gets a new value for it
    ; (stream sid 'arg)
    (->! self 'stream (lambda (args) (let* (
                                            (evargs ((-> self 'ev) args))
                                            (i (car evargs))
                                            (s (-> (-> self 'refs) i))
                                            (r ((-> self 'evl) ((-> self 'unq) s)))
                                            )
                                       (if (->has? (-> self 'store) i)
                                           ; something in the store. get a new value (r), return the old one (r2)
                                           (let (
                                                 (r2 (car (-> (-> self 'store) i)))
                                                 )
                                             (->! (-> self 'store) i (append (-> (-> self 'store) i) (list r)))
                                             (->! (-> self 'store) i (cdr (-> (-> self 'store) i)))
                                             r2
                                             )
                                           ; nothing in the store. Get new value (r), return it, get another (r2), keep that
                                           (let (
                                                 (r2 43);((-> self 'evl) ((-> self 'unq) s)))
                                                 )
                                             ;(->! (-> self 'store) i (list r2))
                                             r
                                             )
                                           )
                                       
                                       ;my $s=%.refs{$i};
                                       ;@.store[$i].push($r);            
                                       ;@.store[$i].shift();            
                                       )))
    
    (->! self 'peek (lambda (args) (let* (
                                          (evargs ((-> self 'ev) args))
                                          (i (car evargs))
                                          )
                                     (car (-> (-> self 'store) i))
                                     )))
    
    (->! self 'iter (lambda (args) (let* (
                                          (evargs ((-> self 'ev) args))
                                          (niters (car evargs))
                                          (task (cadr evargs))
                                          )
                                     (letrec (
                                              (loop (lambda (lst n) (if (< n 1) 
                                                                        lst 
                                                                        (loop
                                                                         (append lst (list ((-> self 'evl) ((-> self 'unq) task))))
                                                                         (- n 1)
                                                                         )
                                                                        )
                                                      )
                                                    ))
                                       
                                       (loop (list) niters)
                                       )
                                     ;for 1..$niters {
                                     ;    @res.push( (task) )
                                     ;}
                                     )))
    (->! self 'eos (lambda (args) (let* ( 
                                         (evargs ((-> self 'ev) args))
                                         (i (car evargs))
                                         )
                                    (eq? (car (-> (-> self 'store) i)) 'undef)                                          
                                    )))
    self
    )
  )

; IO
; I think it is best to have separate cores for different IO channels. So this is similar to FIFO.
; Also, parallel access to a single IO channel does not make sense. So no locks here.
(define (StreamIO)
  (let (
        (self (Service))
        )
    (->! self 'store (new))    
    (->! self 'ioread (lambda (args) (let* ( 
                                            (evargs ((-> self 'ev) args))
                                            (port (car evargs))
                                            )
                                       (if (->has? (-> self 'store) port)                                        
                                           (let (
                                                 (fd (-> (-> self 'store) port))
                                                 )
                                             (if (eof-object? (peek-char fd))
                                                 'undef
                                                 (read-line fd)
                                                 )
                                             )
                                           (let ()
                                       (display port (current-error-port))
                                       (display ": " (current-error-port))
                                           (read-line)
                                             )
                                           )                            
                                       )))
    (->! self 'iowrite (lambda (args) (let* ( 
                                             (evargs ((-> self 'ev) args))
                                             (port (car evargs))
                                             (data (cadr evargs))
                                             )
                                      ;  (display port (current-error-port))
                                      ;  (display ": " (current-error-port))
                                        (if (eq? data 'undef) 
                                            0
                                            (if (->has? (-> self 'store) port) 
                                               (let ((fd (-> (-> self 'store) port))) (display data fd) (newline fd) 1)
                                               (let () (display data) (newline) 1)
                                                ))
                                        )))
    
    (->! self 'display (lambda (args) (let ( 
                                            (evargs ((-> self 'ev) args))
                                            )
                                        (display ">>>")
                                        (map (lambda (item) (display item)
                                               (display ";")) evargs) 
                                        (newline) 
                                        1
                                        )))
    (->! self 'fopen (lambda (args) (let* ( 
                                           (evargs ((-> self 'ev) args))
                                           (port (car evargs))
                                           (filename (cadr evargs))
                                           (mode 1)
                                           )
                                      (set! mode (if (eq? (length evargs) 3) (if (string=? (caddr evargs) "w") 0 1) 1)) 
                                      (->! (-> self 'store) port
                                           (if (eq? mode 1)
                                               (open-input-file filename)
                                               (open-output-file filename)
                                               ))
                                      port
                                      )))
        (->! self 'fclose (lambda (args) (let* ( 
                                           (evargs ((-> self 'ev) args))
                                           (port (car evargs))
                                           (fd (-> (-> self 'store) port))
                                           )
                                           (if (input-port? fd)
                                            (close-input-port fd)
                                            (close-output-port fd)
                                            )
                                      )))
    
    self
    ))
