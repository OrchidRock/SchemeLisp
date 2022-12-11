;:
;: LAZY EVALUTOR
;:

(load "environment.scm")
(load "grammars.scm")
(load "debug_print.scm")

(define (eval expr env)
  (display-debug "eval: ")
  (print-exp expr)
  ;(display-debug "pair?: " (application? expr))
  (newline-debug)
  (cond ((self-evaluating? expr) expr)
        ((variable? expr) (lookup-variable-value expr env))
        ((quoted? expr) (quoted->lazy-list expr env))
        ((assignment? expr) (eval-assignment expr env))
        ((definition? expr) (eval-definition expr env))
        ((if? expr) (eval-if expr env))
        ((lambda? expr)
            (make-procedure (lambda-parameters expr)
                         (lambda-body expr)
                         env))
        ((begin? expr)
            (eval-sequence (begin-actions expr) env))
        ((cond? expr) (eval (cond->if expr) env))

        ((and? expr) (eval-and expr env))
        ((or? expr) (eval-or expr env))

        ;; show how to implement 'and and 'or as derived exprressions.
        ((and? expr) (eval (and->if expr) env))
        ((or? expr) (eval (or-if expr) env))

        ((let? expr) (eval (let->combination expr) env))
        ((let*? expr) (eval (let*->nested-lets expr) env))

        ((while? expr) (eval (while->combination expr) env))

        ((application? expr)
            (apply (actual-value (operator expr) env)
                (operands expr)
                env))
        (else
         (error-report "Unknown exprression type -- EVAL" expr))))

(define (apply procedure arguments env)
    (cond ((primitive-procedure? procedure)
                (apply-primitive-procedure procedure
                                           (list-of-arg-values arguments env)))
          ((compound-procedure? procedure)
                (display-debug "apply-compound-procedure :")
                (display-debug (procedure-parameters procedure))
                (display-debug " ")
                (display-debug (procedure-body procedure))
                (display-debug "  ")
                (user-print-objects arguments)
                ;(display-debug arguments)
                (newline-debug)

                (eval-sequence
                    (procedure-body procedure)
                    (extend-environment
                        (procedure-parameters procedure)
                        (list-of-delayed-args arguments env)
                        (procedure-environment procedure))))
          (else (error-report "Unknown procedure type -- APPLY" procedure))))

(define (quoted->lazy-list expr env)
    (define (get-lazy-list t)
        (if (null? t)
            (list 'quote '())
            (list 'cons (list 'quote (car t)) (get-lazy-list (cdr t)))))
    (let ((text (text-of-quotation expr)))
        (if (pair? text)
            (eval (get-lazy-list text) env)
            text)))

(define (eval-sequence exprs env)
  (cond ((last-exp? exprs) (eval (first-exp exprs) env))
        (else (actual-value (first-exp exprs) env)
              (eval-sequence (rest-exps exprs) env))))

(define (eval-if expr env)
    (if (true? (actual-value (if-predicate expr) env))
        (eval (if-consequent expr) env)
        (eval (if-alternative expr) env)))

(define (actual-value expr env)
    (display-debug "actual-value: ")
    (print-exp expr)
    (newline-debug)
    (force-it (eval expr env)))

(define (list-of-arg-values exprs env)
    (if (no-operands? exprs)
        '()
        (cons (actual-value (first-operand exprs) env)
              (list-of-arg-values (rest-operands exprs) env))))

(define (list-of-delayed-args exprs env)
    (if (no-operands? exprs)
        '()
        (cons (delay-it (first-operand exprs) env)
              (list-of-delayed-args (rest-operands exprs) env))))


;: Representing thunks
(define (force-it obj)
    (if (thunk? obj)
        (begin
            (display-debug "force thunk: ")
            (print-exp (thunk-exp obj))
            (newline-debug)
            (actual-value (thunk-exp obj) (thunk-env obj)))
        obj))

;: Memozied thunk
(define (evaluated-thunk? obj) (tagged-list? obj 'evaluated-thunk))
(define (thunk-value evaluated-thunk) (cadr evaluated-thunk))

(define (force-it obj)
    (cond ((thunk? obj)
            (display-debug "force start => ")
            (print-exp (thunk-exp obj))
            (newline-debug)
            (let ((result (actual-value (thunk-exp obj) (thunk-env obj))))

                (display-debug "forced thunk: ")
                (print-exp (thunk-exp obj))
                (display-debug " => ")
                (print-exp result)
                (newline-debug)

                (set-car! obj 'evaluated-thunk)
                (set-car! (cdr obj) result) ;: replase expr with its value.
                (set-cdr! (cdr obj) '()) ;: forgrt unneeded env.
                result))
          ((evaluated-thunk? obj)

            (display-debug "forced evaluated-thunk: ")
            (print-exp (thunk-exp obj))
            (display-debug " => ")
            (print-exp (thunk-value obj))
            (newline-debug)

            (thunk-value obj))
          (else obj)))


(define (delay-it expr env)
    (display-debug "delay: ")
    (print-exp expr)
    (newline-debug)
    (list 'thunk expr env))

(define (thunk? obj) (tagged-list? obj 'thunk))
(define (thunk-exp thunk) (cadr thunk))
(define (thunk-env thunk) (caddr thunk))

(define (print-exp expr)
    (if (thunk? expr)
        (begin (display-debug "(thunk ")
               (display-debug (thunk-exp expr))
               (display-debug " <env>)"))
        (user-print expr)))

(define (user-print object)
  (cond ((compound-procedure? object)
            (display-debug (list 'compound-procedure
                           (procedure-parameters object)
                           (procedure-body object)
                           '<procedure-env>)))
        ((thunk? object)
            (print-exp object))
        ((evaluated-thunk? object)
            (print-exp (thunk-value object)))
        (else (display-debug object))))

(define (load-from-file file)
    (define (loop-read port)
        (let ((input (read port)))
            (if (eof-object? input)
                'ok
                (begin (actual-value input the-global-environment )
                  (loop-read port)))))
    (loop-read (open-input-file file)))


(define input-prompt ";;; Lazy-Eval input:")
(define output-prompt ";;; Lazy-Eval value:")

(define primitive-procedures
  (list (list 'load load-from-file)
        (list 'null? null?)
        (list '+ +)
        (list '- -)
        (list '* *)
        (list '/ /)
        (list '= =)
        (list 'square square)
        ;(list 'map map)
        (list 'newline newline)
        (list 'display display)
        (list 'list list)
        ;(list 'car car)
        ;(list 'cdr cdr)
        ;(list 'cons cons)

;;      more primitives
        ))



(define (driver-loop)
    (prompt-for-input input-prompt)
    (let ((input (read)))
        (let ((output
                (actual-value input the-global-environment)))
            (announce-output output-prompt)
            (user-print-output output)))
    (driver-loop))


(define the-global-environment (setup-environment))
;(actual-value '(define (cons x y) (lambda (m) (m x y))) the-global-environment)
;(actual-value '(define (car z) (z (lambda (p q) p))) the-global-environment)
;(actual-value '(define (cdr z) (z (lambda (p q) q))) the-global-environment)
(driver-loop)
