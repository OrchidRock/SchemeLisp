
;:
;: Amb Evaluator

(load "debug_print.scm")
(load "prime.scm")
(load "grammars.scm")
(load "environment.scm")

(define (ambeval expr env succeed fail)
    (display-debug "ambeval: ")
    (display-debug expr)
    (display-debug " <env> ")
    (display-debug succeed)
    (display-debug " ")
    (display-debug fail)
    (newline-debug)
    ((analyze expr) env succeed fail))

(define (analyze expr)
  (cond ((self-evaluating? expr)
         (analyze-self-evaluating expr))
        ((quoted? expr) (analyze-quoted expr))
        ((variable? expr) (analyze-variable expr))
        ((assignment? expr) (analyze-assignment expr))
        ((permanent-assignment? expr) (analyze-perm-assignment expr))
        ((definition? expr) (analyze-definition expr))
        ((if? expr) (analyze-if expr))
        ((lambda? expr) (analyze-lambda expr))
        ((begin? expr) (analyze-sequence (begin-actions expr)))
        ((cond? expr) (analyze (cond->if expr)))

        ((and? expr) (analyze (and->if expr)))
        ((or? expr) (analyze (or->if expr)))

        ((let? expr) (analyze-let expr))
        ((unless? expr) (analyze (unless->if expr)))

        ((amb? expr) (analyze-amb expr))

        ((application? expr) (analyze-application expr))
        (else
            (error-report "Unknown exprression type -- ANALYZE" expr))))

(define (print-analyze msg . objects)
    (display-debug msg)
    (if (null? objects)
        'ok
        (display-debug objects))
    (newline-debug))


(define (analyze-self-evaluating expr)
    (print-analyze "Analyze-self-evaluating <=> " expr)
    (lambda (env succeed fail)
        (succeed expr fail)))
(define (analyze-quoted expr)
    (print-analyze "Before analyze-quoted => " expr)
    (let ((qval (text-of-quotation expr)))
        (print-analyze "=> After analyze-quoted: lambda (env succeed fail)" qval)
        (lambda (env succeed fail)
            (succeed qval fail))))

(define (analyze-variable expr)
    (print-analyze "Analyze-variable <=> " expr)
    (lambda (env succeed fail)
        (succeed (lookup-variable-value expr env) fail)))

(define (analyze-lambda expr)
    (print-analyze "Before analyze-lambda => " expr)
    (let ((vars (lambda-parameters expr))
          (bproc (analyze-sequence (lambda-body expr))))
        (print-analyze "=> After analyze-lambda: lambda (env succeed fail) " vars bproc)
        (lambda (env succeed fail)
            (succeed (make-procedure vars bproc env) fail))))

(define (analyze-if expr)
    (print-analyze "Before analyze-if => " expr)
    (let ((pproc (analyze (if-predicate expr)))
          (cproc (analyze (if-consequent expr)))
          (aproc (analyze (if-alternative expr))))
        (print-analyze "=> After analyze-if: lambda (env succeed fail) " pproc cproc aproc)
        (lambda (env succeed fail)
            (pproc env
                   (lambda (pred-value fail2)
                        (if (true? pred-value)
                            (cproc env succeed fail2)
                            (aproc env succeed fail2)))
                    fail))))

(define (analyze-sequence exprs)
    (print-analyze "Before analyze-sequence => " exprs)
    (define (sequentially a b)
        (lambda (env succeed fail)
            (a env
                (lambda (a-value fail2)
                    (b env succeed fail2))
                fail)))
    (define (loop first-proc rest-procs)
        (print-analyze "  analyze-sequence loop => first-proc: " first-proc)
        (if (null? rest-procs)
            (begin (print-analyze "=> After analyze-sequence: " first-proc)
                first-proc)
            (loop (sequentially first-proc (car rest-procs))
                  (cdr rest-procs))))
    (let ((procs (map analyze exprs)))
        (print-analyze  "  Before analyze-sequence loop => " procs)
        (if (null? procs)
            (error-report "Empty sequence -- ANALYZE"))
        (loop (car procs) (cdr procs))))

(define (analyze-definition expr)
    (print-analyze "Before analyze-definition => " expr)
    (let ((var (definition-variable expr))
          (vproc (analyze (definition-value expr))))
        (print-analyze "=> After analyze-definition: lambda (env succeed fail) " var vproc)
        (lambda (env succeed fail)
            (vproc env
                    (lambda (val fail2)
                        (define-variable! var val env)
                        (succeed 'ok fail2))
                   fail))))
(define (analyze-assignment expr)
    (print-analyze "Before analyze-assignment => " expr)
    (let ((var (assignment-variable expr))
          (vproc (analyze (assignment-value expr))))
        (print-analyze "=> After analyze-assignment: lambda (env) " var vproc)
        (lambda (env succeed fail)
            (vproc env
                    (lambda (val fail2)
                        (let ((old-vlue (lookup-variable-value var env)))
                            (set-variable-value! var val env)
                            (succeed 'ok
                                     (lambda ()
                                        (set-variable-value! var old-vlue env)
                                        (fail2)))))
                    fail))))

(define (permanent-assignment? expr)
  (tagged-list? expr 'permanent-set!))
(define (analyze-perm-assignment expr)
    (print-analyze "Before analyze-perm-assignment => " expr)
    (let ((var (assignment-variable expr))
          (vproc (analyze (assignment-value expr))))
        (print-analyze "=> After analyze-perm-assignment: lambda (env) " var vproc)
        (lambda (env succeed fail)
            (vproc env
                    (lambda (val fail2)
                        (set-variable-value! var val env)
                        (succeed 'ok fail2))
                    fail))))


(define (analyze-let expr)
    (print-analyze "Before analyze-let => " expr)
    (let ((proc (analyze (let->combination expr))))
        (print-analyze "=> After analyze-let: lambda (env succeed fail) " proc)
        proc))
        ;(lambda (env succeed fail)
        ;    (proc env succeed fail))))

(define (analyze-application expr)
    (print-analyze "Before analyze-application => " expr)
    (let ((fproc (analyze (operator expr)))
          (aprocs (map analyze (operands expr))))
        (print-analyze "=> After analyze-application: lambda (env succeed fail) " fproc aprocs)
        (lambda (env succeed fail)
            (fproc env
                    (lambda (proc fail2)
                        (get-args aprocs env (lambda (args fail3)
                                                (execute-application proc args succeed fail3))
                                             fail2))
                    fail))))
(define (get-args aprocs env succeed fail)
    (print-analyze "Before get-args => " aprocs)
    (if (null? aprocs)
        (succeed '() fail)
        ((car aprocs) env
                      (lambda (arg fail2)
                            (get-args (cdr aprocs)
                                      env
                                      (lambda (args fail3)
                                        (print-analyze "=> After get-args: " args)

                                        (succeed (cons arg args) fail3))
                                       fail2))
                       fail)))

(define (execute-application proc args succeed fail)
    (cond ((primitive-procedure? proc)
            (succeed (apply-primitive-procedure proc args) fail))
          ((compound-procedure? proc)
            ((procedure-body proc)
                (extend-environment (procedure-parameters proc)
                                    args
                                    (procedure-environment proc))
                 succeed
                 fail))
          (else (error-report "Unknown procedure type -- EXECUTE-APPLICATION" proc))))

(define (amb? expr) (tagged-list? expr 'amb))
(define (amb-choices expr) (cdr expr))
(define (analyze-amb expr)
    (print-analyze "Before analyze amb => ")
    (let ((cprocs (map analyze (amb-choices expr))))
        (print-analyze "=> After analyze amb:  " cprocs)
        (lambda (env succeed fail)
            (define (try-next choices)
                (if (null? choices)
                    (fail)
                    ((car choices) env
                                   succeed
                                   (lambda () (try-next (cdr choices))))))
            (try-next cprocs))))


(define input-prompt ";;; Amb-Eval input:")
(define output-prompt ";;; Amb-Eval value:")

(define (get-input)
    (if (null? pre-eval-procedures)
        (read)
        (let ((first-proc (car pre-eval-procedures)))
            (set! pre-eval-procedures (cdr pre-eval-procedures))
            first-proc)))

(define (driver-loop)
  (define (internal-loop try-again)
    (prompt-for-input input-prompt)
    (let ((input (get-input)))
      (if (eq? input 'try-again)
          (try-again)
          (begin
            (newline)
            (display ";;; Starting a new problem ")
            (ambeval input
                     the-global-environment
                     ;; ambeval success
                     (lambda (val next-alternative)
                       (announce-output output-prompt)
                       (user-print-output val)
                       (internal-loop next-alternative))
                     ;; ambeval failure
                     (lambda ()
                       (announce-output
                        ";;; There are no more values of")
                       (user-print-output input)
                       (driver-loop)))))))
  (internal-loop
   (lambda ()
     (newline)
     (display ";;; There is no current problem")
     (driver-loop))))


(define (load-from-file file)
    (define (loop-read port)
        (let ((input (read port)))
            (if (eof-object? input)
                'ok
                (begin (ambeval input the-global-environment
                                (lambda (x y) ())
                                (lambda () ()))
                  (loop-read port)))))
    (loop-read (open-input-file file)))

(define primitive-procedures
  (list (list 'car car)
        (list 'cdr cdr)
        (list 'cons cons)
        (list 'null? null?)
        (list 'list list)
        (list 'memq memq)
        (list 'member member)
        (list 'not not)
        (list '+ +)
        (list '- -)
        (list '* *)
        (list '= =)
        (list '> >)
        (list '>= >=)
        (list '<= <=)
        (list 'abs abs)
        (list 'remainder remainder)
        (list 'integer? integer?)
        (list 'sqrt sqrt)
        (list 'eq? eq?)
        (list 'prime? prime?) ;: load prime.scm
        (list 'display display)
        (list 'newline newline)
        (list 'load load-from-file)
        ;(list 'and and)
        ;(list 'or or)
        ;(list 'read-string read-string)
        ;(list 'open-i/o-file open-i/o-file)
;;      more primitives
        ))

(define pre-eval-procedures
    (list '(define (require p) (if (not p) (amb)))))

(define the-global-environment (setup-environment))
(driver-loop)
