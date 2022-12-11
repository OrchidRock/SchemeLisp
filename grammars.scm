
;: Rewrite eval so that the dispatch is done in data-directed style.
;; (load "../chap2/ata_directed_package.scm")


(define (error-no-exception msg . others)
    (display-debug msg)
    (display-debug " ")
    (display-debug others)
    (newline-debug)
    -1)
;:
;: We can rewrite "list-of-values" to implement the "simultaneous" scope rule.
;: But we have to consider the procedure, which includes mutually recursive internal
;: definitions.

(define (list-of-values-new exprs env) ())

(define (list-of-values exprs env)
  (if (no-operands? exprs)
      '()
      (cons (eval (first-operand exprs) env)
            (list-of-values (rest-operands exprs) env))))

;: A version of "list-of-values" that evaluates operands from left to right regardless of the
;: order of evalution in the underlying Lisp.
(define (list-of-values-left-to-right exprs env)
    (define (iter e results)
        (if (no-operands? e)
            results
            (let ((left-value (eval (first-operand e) env)))
                (iter (rest-operands e) (append results (list left-value))))))
    (iter exprs '()))
(define (list-of-values-left-to-right2 exprs env)
    (if (no-operands? exprs)
        '()
        (let ((left-value (eval (first-operand exprs) env)))
            (cons left-value (list-of-values-left-to-right2
                                    (rest-operands exprs)
                                    env)))))

;: A version of list-of-values that evaluates operands from right to left regardless of the
;: order of evalution in the underlying Lisp.
(define (list-of-values-right-to-left exprs env)
    (if (no-operands? exprs)
        '()
        (let ((right-value (list-of-values-right-to-left (rest-operands exprs) env)))
            (append right-value (list (eval (first-operand exprs) env))))))

;: The semantics of "true?" can be defined by ourself instead of
;: underlying scheme.
(define (eval-if expr env)
  (if (true? (eval (if-predicate expr) env))
      (eval (if-consequent expr) env)
      (eval (if-alternative expr) env)))

(define (eval-sequence exprs env)
  (cond ((last-exp? exprs) (eval (first-exp exprs) env))
        (else (eval (first-exp exprs) env)
              (eval-sequence (rest-exps exprs) env))))

(define (eval-assignment expr env)
  (set-variable-value! (assignment-variable expr)
                       (eval (assignment-value expr) env)
                       env)
  'ok)

(define (eval-definition expr env)
  (define-variable! (definition-variable expr)
                    (eval (definition-value expr) env)
                    env)
  'ok)


(define (self-evaluating? expr)
  (cond ((number? expr) true)
        ((string? expr) true)
        ((eq? expr '*unassigned*) '*unassigned*)
        (else false)))

;: (quote <text-of-quotation>) is equal to '<text--of-quotation> .
(define (quoted? expr)
  (tagged-list? expr 'quote))

(define (text-of-quotation expr) (cadr expr))

;:
(define (install-quoted-package)
    (define (text-of-quotation-env expr env)
        (text-of-quotation expr))
    (put 'eval 'quote text-of-quotation-env))
;:

(define (tagged-list? expr tag)
  (if (pair? expr)
      (eq? (car expr) tag)
      false))

(define (variable? expr) (symbol? expr))


;: Assignment: (set! <var> <value>)
(define (make-assignment var val)
    (cons 'set! (list var val)))

(define (assignment? expr)
  (tagged-list? expr 'set!))

(define (assignment-variable expr) (cadr expr))
(define (assignment-value expr) (caddr expr))


(define (install-assignment-package)
    (put 'eval 'set! eval-assignment))


;: Definition statement:
(define (definition? expr)
  (tagged-list? expr 'define))

;: Example: (define VAR "hello")
;: or (define (func x) (* x 2))
(define (definition-variable expr)
  (if (symbol? (cadr expr))
      (cadr expr)
      (caadr expr)))

(define (definition-value expr)
  (if (symbol? (cadr expr))
      (caddr expr)
      ;: function definition.
      (make-lambda (cdadr expr)     ;: format parameters
                   (cddr expr))))   ;: body

(define (make-definition variable parameters body)
    (cons 'define (cons (cons variable parameters)
                        body)))


(define (install-definition-package)
    (put 'eval 'define eval-definition))
(define (install-lambda-package)
    (define (make-procedure-from-lambda expr env)
        (make-procedure (lambda-parameters expr) (lambda-body expr) env))
    (put 'eval 'lambda make-procedure-from-lambda))


;: Lambda expression.
(define (lambda? expr) (tagged-list? expr 'lambda))

(define (lambda-parameters expr) (cadr expr))
(define (lambda-body expr) (cddr expr))

(define (make-lambda parameters body)
  (cons 'lambda (cons parameters body)))


(define (install-if-package)
    (put 'eval 'if eval-if))

;: if expression:
(define (if? expr) (tagged-list? expr 'if))

(define (if-predicate expr) (cadr expr))
(define (if-consequent expr) (caddr expr))

(define (if-alternative expr)
  (if (not (null? (cdddr expr)))
      (cadddr expr)
      'false))

(define (make-if predicate consequent alternative)
  (list 'if predicate consequent alternative))


(define (install-begin-package)
    (define (my-begin expr env)
        (eval-sequence (begin-actions expr) env))
    (put 'eval 'begin my-begin))

;: begin statement:
(define (begin? expr) (tagged-list? expr 'begin))

(define (begin-actions expr) (cdr expr))

(define (last-exp? seq) (null? (cdr seq)))
(define (first-exp seq) (car seq))
(define (rest-exps seq) (cdr seq))


;: convert sequence to  expression:
;: for example:
(define (sequence->exp seq)
  (cond ((null? seq) seq)
        ((last-exp? seq) (first-exp seq))
        (else (make-begin seq))))

(define (make-begin seq) (cons 'begin seq))


;: (call <operator> <operands>)
;: (define (application? expr) (tagged-list? expr 'call))
;: (define (operator expr) (cadr expr))
;: (define (operands expr) (cddr expr))

;: "application" is the compound-expressions which are
;: not belong to any above expressions.
(define (install-application-package)
    (define (my-application expr env)
        (apply (eval (operator expr) env) (list-of-values (operands expr) env)))
    (put 'eval 'call my-application))

(define (application? expr) (pair? expr))
(define (operator expr) (car expr))
(define (operands expr) (cdr expr))


(define (no-operands? ops) (null? ops))
(define (first-operand ops) (car ops))
(define (rest-operands ops) (cdr ops))


(define (install-cond-package)
    (define (my-cond expr env)
        (eval (cond->if expr) env))
    (put 'eval 'cond my-cond))


;: Cond expression is a "derived expression" as syntax suger
;: which can be derived by "if" expressions .
(define (cond? expr) (tagged-list? expr 'cond))

(define (cond-clauses expr) (cdr expr))

(define (cond-else-clause? clause)
  (eq? (cond-predicate clause) 'else))

;: The extend syntax: (<test> => <recipient>)
(define (cond-form2-clause? clause)
    (eq? (car (cond-actions clause)) '=>))

;; only one action which has only one parameter.
(define (cond-form2-action clause)
    (cadr (cond-actions clause)))

(define (cond-predicate clause) (car clause))
(define (cond-actions clause) (cdr clause))

(define (cond->if expr)
  (expand-clauses (cond-clauses expr)))

(define (expand-clauses clauses)
  (if (null? clauses)
      'false                          ; no else clause
      (let ((first (car clauses))
            (rest (cdr clauses)))
        (if (cond-else-clause? first)
            (if (null? rest)
                (sequence->exp (cond-actions first))
                (error-report "ELSE clause isn't last -- COND->IF"
                       clauses))

            (if (and (cond-form2-clause? first)
                     (cond-predicate first))
                (list (cond-form2-action first) (cond-predicate first));
                ;(eval (cond-form2-action first) (cond-predicate first))
                (make-if (cond-predicate first)
                         (sequence->exp (cond-actions first))
                         (expand-clauses rest)))))))


;: And statement: (and <expr1> <expr2>...)
;: Or statement: (or <expr1> <expr2>...)
(define (and? expr) (tagged-list? expr 'and))
(define (or? expr) (tagged-list? expr 'or))
(define (eval-and expr env)
    (define (eval-and-caluses caluses)
        (if (null? caluses)
            'true
            (if (eval (car caluses) env)
                (eval-and-caluses (cdr caluses))
                'false)))
    (eval-and-caluses (cdr expr)))

(define (eval-or expr env)
    (define (eval-or-caluses caluses)
        (if (null? caluses)
            'false
            (if (eval (car caluses) env)
                'true
                (eval-or-caluses (cdr caluses)))))
    (eval-or-caluses (cdr expr)))

;: "and" or "or" statement can alse be derived by "if" statement
;: like "cond".
(define (and->if expr)
    (define (expand-and-caluses caluses)
        (if (null? caluses)
            'true
            (make-if (car caluses)
                     (expand-and-caluses (cdr caluses))
                     'false)))
    (expand-and-caluses (cdr expr)))

(define (or->if expr)
    (define (expand-or-caluses caluses)
        (if (null? caluses)
            'false
            (make-if (car caluses)
                     'true
                     (expand-or-caluses (cdr caluses)))))
    (expand-or-caluses (cdr expr)))

(define (install-or-package)
    (put 'eval 'or eval-or))
(define (install-and-package)
    (put 'eval 'and eval-and))


;: let statement:
;: it can be derived by "lambda" statement .
;: "let*" is similar with "let" but it's variable-constraint
;: finished by the order from left to right.
(define (let*? expr) (tagged-list? expr 'let*))
(define (make-let parameters-and-exps body)
    (cons 'let (cons parameters-and-exps body)))

(define (let*->nested-lets expr)
    (define (translate caluses)
        (if (null? caluses)
            (let-body expr)
            (make-let (list (car caluses))
                      (translate (cdr caluses)))))
    (translate (let-vars-and-exps expr)))

(define (let? expr) (tagged-list? expr 'let))
(define (let-body expr) (cddr expr))
(define (let-vars-and-exps expr) (cadr expr))
(define (let-vars caluses)
    (if (null? caluses)
        '()
        (cons (car (car caluses))
              (let-vars (cdr caluses)))))
(define (let-exps caluses)
    (if (null? caluses)
        '()
        (cons (cadr (car caluses))
              (let-exps (cdr caluses)))))

(define (named-let? expr) (variable? (cadr expr)))

(define (let->combination expr)
    (if (named-let? expr)
        (make-begin (list (make-definition (cadr expr)
                               (let-vars (caddr expr))
                               (cdddr expr)))
                          (cons (cadr expr) (let-exps (caddr expr)))))
        (cons (make-lambda (let-vars (let-vars-and-exps  expr))
                           (let-body expr))
              (let-exps (let-vars-and-exps expr))))

(define (install-let-package)
    (define (my-let expr env)
        (eval (let->combination expr) env))
    (put 'eval 'let my-let))


;: while statement:
(define (while? expr) (tagged-list? expr 'while))
(define (while-predicate expr) (cadr expr))
(define (while-body expr) (cddr expr))
;(define (make-while predicate body)
;    (cons 'while (cons predicate body)))

(define (while->combination expr)
    (make-if (while-predicate expr)
             (make-begin (append (while-body expr) (list expr)))
             'true))

;: unless statement:
(define (unless? expr) (tagged-list? expr 'unless))
(define (unless-condition expr) (cadr expr))
(define (unless-usual-value expr) (caddr expr))
(define (unless-exceptional-value expr) (cadddr expr))
(define (unless->if expr)
    (make-if (unless-condition expr)
             (unless-exceptional-value expr)
             (unless-usual-value expr)))



;: evaluator data structures
(define (true? x)
  (not (eq? x false)))

(define (false? x)
  (eq? x false))

;: scan-out-defines constructes a extra frame.
;: Design a way without constructing it:
;: To translate all internal definitations to lambda exprressions will create new problem
;: that which breaks the order of set!.
(define (scan-out-defines-optim proc-body)
    (define (iter body define-caluses set-caluses expr-calues)
        (if (null? body)
            (cons define-caluses (append set-caluses expr-calues))
            (let ((first-exp (car body)))
                (if (definition? first-exp)
                    (iter (cdr body)
                          (append define-caluses (list (list 'define
                                                             (definition-variable first-exp)
                                                             '*unassigned*)))
                          (append set-caluses (list (make-assignment (definition-variable first-exp)
                                                 (definition-value first-exp))))
                          expr-calues)
                    (iter (cdr body)
                          define-caluses
                          set-caluses
                          (append expr-calues (list first-exp)))))))
    (let ((items (iter proc-body '() '() '())))

        (if (null? (car items))
            proc-body
            (append (car items) (cdr items)))))

(define (scan-out-defines proc-body)
    (define (iter body let-caluses set-caluses expr-calues)
        (if (null? body)
            (cons let-caluses (append set-caluses expr-calues))
            (let ((first-exp (car body)))
                (if (definition? first-exp)
                    (iter (cdr body)
                          (append let-caluses (list (list (definition-variable first-exp)
                                                          '*unassigned*)))
                          (append set-caluses (list (make-assignment (definition-variable first-exp)
                                                 (definition-value first-exp))))
                          expr-calues)
                    (iter (cdr body)
                          let-caluses
                          set-caluses
                          (append expr-calues (list first-exp)))))))
    (let ((items (iter proc-body '() '() '())))

        (if (null? (car items))
            proc-body
            (list (make-let (car items) (cdr items))))))

;:
;: scan-out-defines can't match the analyzing_mceval.scm
;:
(define (make-procedure parameters body env)
    (display-debug "make-procedure scan-out-defines: ")
    (display-debug body)
    (newline-debug)
    (display-debug "           after: ")
    ;(display-debug (scan-out-defines-optim body))
    (newline-debug)
    ;(list 'procedure parameters (scan-out-defines-optim body) env))
    ;(list 'procedure parameters (scan-out-defines body) env))
    (list 'procedure parameters body env))


;: compound-procedure:
(define (compound-procedure? p)
  (tagged-list? p 'procedure))

(define (procedure-parameters p) (cadr p))
(define (procedure-body p) (caddr p))
(define (procedure-environment p) (cadddr p))

(define (primitive-procedure? proc)
  (tagged-list? proc 'primitive))

(define (primitive-implementation proc) (cadr proc))



