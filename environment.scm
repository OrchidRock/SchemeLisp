
;: Keep reverse for the underlying "apply" procedure.
(define apply-in-underlying-scheme apply)

;: Environments
(define (enclosing-environment env) (cdr env))
(define (first-frame env) (car env))
(define the-empty-environment '())

(define (make-frame variables values)
  (cons variables values))

(define (frame-variables frame) (car frame))
(define (frame-values frame) (cdr frame))

(define (add-binding-to-frame! var val frame)
  (set-car! frame (cons var (car frame)))
  (set-cdr! frame (cons val (cdr frame))))

(define (extend-environment vars vals base-env)
  (display "extend-environment: ")
  (display vars)
  (display "  ")
  (user-print-objects vals)
  (newline)
  (if (= (length vars) (length vals))
      (cons (make-frame vars vals) base-env)
      (if (< (length vars) (length vals))
          (error "Too many arguments supplied" vars vals)
          (error "Too few arguments supplied" vars vals))))

;: This constraint way of our environment is simple but inefficient.
(define (lookup-variable-value var env)
  (display "lookup-variable-value: ")
  (display var)
  (display " => ")
  (define (env-loop env)
    (define (scan vars vals)
      (cond ((null? vars)
             (env-loop (enclosing-environment env)))
            ((eq? var (car vars))
             (if (eq? (car vals) '*unassigned*) ;:
                (error "LOOKUP-VARIABLE-VALUE have found unassigned variable" var)
                (car vals)))
            (else (scan (cdr vars) (cdr vals)))))
    (if (eq? env the-empty-environment)
        (error "Unbound variable" var)
        (let ((frame (first-frame env)))
          (scan (frame-variables frame)
                (frame-values frame)))))
  (let ((return-val (env-loop env)))
    (user-print return-val)
    (newline)
    return-val))

(define (set-variable-value! var val env)
  (display "set-variable-value!: ")
  (display var)
  (display " ")
  (user-print val)
  (newline)
  (define (env-loop env)
    (define (scan vars vals)
      (cond ((null? vars)
             (env-loop (enclosing-environment env)))
            ((eq? var (car vars))
             (set-car! vals val))
            (else (scan (cdr vars) (cdr vals)))))
    (if (eq? env the-empty-environment)
        (error "Unbound variable -- SET!" var)
        (let ((frame (first-frame env)))
          (scan (frame-variables frame)
                (frame-values frame)))))
  (env-loop env))

(define (define-variable! var val env)
  (display "define-variable!: ")
  (display var)
  (display " ")
  (user-print val)
  (newline)
  (let ((frame (first-frame env)))
    (define (scan vars vals)
      (cond ((null? vars)
             (add-binding-to-frame! var val frame))
            ((eq? var (car vars))
             (set-car! vals val))
            (else (scan (cdr vars) (cdr vals)))))
    (scan (frame-variables frame)
          (frame-values frame))))


(define primitive-procedures
  (list (list 'car car)
        (list 'cdr cdr)
        (list 'cons cons)
        (list 'null? null?)
        (list '+ +)
        (list '= =)
        (list '> >)
        (list 'square square)
        (list 'quit quit)
        (list 'list list)
        (list 'map map)
;;      more primitives
        ))


;: Setup Environments
(define (setup-environment)
  (let ((initial-env
         (extend-environment (primitive-procedure-names)
                             (primitive-procedure-objects)
                             the-empty-environment)))
    (define-variable! 'true true initial-env)
    (define-variable! 'false false initial-env)
    initial-env))


;: if we input: (map car (list (list 1 2) (list 2 3)))
;: the map object of system: #[compiled-procedure 14 (list #x6f)]
;: the car object of system: #[compiled-procedure 15 (list #x1)]
;: then we apply: (apply-in-underlying-scheme #[compiled-procedure 14 (list #6xf)]
;:                                            ('primitive #[compiled-procedure 15 (list #x1)])
;:                                            ((1 2) (3 4)))
;: the system's map can't identify the ('primitive #[compiled-procedure 15 (list #x1)])
;: because it's the defination of ourself.
;: so we have to define our own map procedure.

(define (primitive-procedure-names) (map car primitive-procedures))
(define (primitive-procedure-objects) (map (lambda (proc) (list 'primitive (cadr proc)))
                                           primitive-procedures))

(define (apply-primitive-procedure proc args)
    (display "apply-primitive-procedure: ")
    (display proc)
    (display "  ")
    (user-print-objects args)
    (newline)
    (apply-in-underlying-scheme (primitive-implementation proc) args))
