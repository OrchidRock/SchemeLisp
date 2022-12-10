;:
;: the frame is a table of cons
;: instead of a cons of two tables.
;:
(define (enclosing-environment env) (cdr env))
(define (first-frame env) (car env))
(define the-empty-environment '())

(define (make-frame variables values)
    (if (or (null? variables) (null? values))
        '()
        (cons (cons (car variables) (car values))
              (make-frame (cdr variables) (cdr values)))))
(define (frame-first-variable frame) (caar frame))
(define (frame-first-value frame) (cdar frame))
(define (add-binding-to-frame! var val frame)
    (set! frame (cons (cons var val) frame)))

(define (extend-environment vars vals base-env)
    (if (= (length vars) (length vals))
        (cons (make-frame vars vals) base-env)
        (if (< (length vars) (length vals))
            (error "Too many arguments supplied" vars vals)
            (error "Too few arguments supplied" vars vals))))

(define (lookup-variable-value var env)
  (define (env-loop env)
    (define (scan f)
      (cond ((null? f)
             (env-loop (enclosing-environment env)))
            ((eq? var (caar f))
             (cdar f))
            (else (scan (cdr f)))))
    (if (eq? env the-empty-environment)
        (error "Unbound variable" var)
        (let ((frame (first-frame env)))
          (scan frame))))
  (env-loop env))

(define (set-variable-value! var val env)
  (define (env-loop env)
    (define (scan f)
      (cond ((null? f)
             (env-loop (enclosing-environment env)))
            ((eq? var (caar f))
             (set-cdr! (car f) val))
            (else (scan (cdr f)))))
    (if (eq? env the-empty-environment)
        (error "Unbound variable -- SET!" var)
        (let ((frame (first-frame env)))
          (scan frame))))
  (env-loop env))

(define (define-variable! var val env)
  (let ((frame (first-frame env)))
    (define (scan f)
      (cond ((null? f)
             (add-binding-to-frame! var val frame))
            ((eq? var (caar f))
             (set-cdr! (car f) val))
            (else (scan (cdr f)))))
    (scan frame)))


;:
;: High-Order Function.
(define (lookup-operate-env-variable var val env f-not-founded f-founded)
    (define (env-loop env)
        (define (scan vars vals)
            (cond ((null? vars) ; not-founded
                (f-not-founded (enclosing-environment env)
                               (lambda (var val) (add-binding-to-frame! var val frame))))
            ((eq? var (car vars))
                (set-car! vals val)
                (car vals))
            (else (scan (cdr vars) (cdr vals)))))
        (if (eq? env the-empty-environment)
            (error "Unbound variable -- SET!" var)
            (let ((frame (first-frame env)))
                (scan (frame-variables frame) (frame-values frame)))))
    (env-loop env))

(define (set-variable-value! var val env)
    (lookup-operate-env-variable
            var
            val
            env
            (lambda (e f) (set-variable-value! var val e))
            (lambda (pair) (set-cdr! pair val))))

(define (define-variable! var val env)
    (lookup-operate-env-variable
            var
            val
            env
            (lambda (e f) (f var val))
            (lambda (pair) (set-cdr! pair val))))

(define (lookup-variable-value var env)
    (lookup-operate-env-variable
            var
            0
            env
            (lambda (e f) (lookup-variable-value var e))
            (lambda (pair) (cdr pair))))
