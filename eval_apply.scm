;:
;: eval-loop
;:

(load "environment.scm")
(load "grammars.scm")

(define (eval expr env)
    (cond ((self-evaluating? expr) expr)
          ((variable? expr) (lookup-variable-value expr env))
          (else ((get 'eval (car expr)) expr env))))

(define (eval expr env)
  (display "eval: ")
  (display expr)
  ;(display "pair?: " (application? expr))
  (newline)
  (cond ((self-evaluating? expr) expr)
        ((variable? expr) (lookup-variable-value expr env))
        ((quoted? expr) (text-of-quotation expr))
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

        ;; show how to implement 'and and 'or as derived expressions.
        ((and? expr) (eval (and->if expr) env))
        ((or? expr) (eval (or->if expr) env))

        ((let? expr) (eval (let->combination expr) env))
        ((let*? expr) (eval (let*->nested-lets expr) env))

        ((while? expr) (eval (while->combination expr) env))
        ((unless? expr) (eval (unless->if expr) env))

        ((application? expr)
         ;(display "lv: " (list-of-values (operands expr) env))
         ;(newline)
         (apply (eval (operator expr) env)
                (list-of-values (operands expr) env)))
        (else
            (error "Unknown exprression type -- EVAL" expr))))

(define (apply procedure arguments)
    (cond ((primitive-procedure? procedure)
                (apply-primitive-procedure procedure arguments))
          ((compound-procedure? procedure)
                (display "apply-compound-procedure :")
                (display (procedure-parameters procedure))
                (display " ")
                (display (procedure-body procedure))
                (display "  ")
                (user-print-objects arguments)
                ;(display arguments)
                (newline)

                (eval-sequence
                    (procedure-body procedure)
                    (extend-environment
                        (procedure-parameters procedure)
                        arguments
                        (procedure-environment procedure))))
          (else (error "Unknown procedure type -- APPLY" procedure))))



(define input-prompt ";;; M-Eval input:")
(define output-prompt ";;; M-Eval value:")

(define (driver-loop)
  (prompt-for-input input-prompt)
  (let ((input (read)))
    (let ((output (eval input the-global-environment)))
      (announce-output output-prompt)
      (user-print output)))
  (driver-loop))

(define (prompt-for-input string)
  (newline) (newline) (display string) (newline))

(define (announce-output string)
  (newline) (display string) (newline))

(define (user-print-objects objects)
    (cond ((pair? objects)
               (user-print (car objects))
               (display " ")
               (user-print-objects (cdr objects)))
          ((null? objects) 'ok)
          (else (user-print objects))))

(define (user-print object)
  (if (compound-procedure? object)
      (display (list 'compound-procedure
                     (procedure-parameters object)
                     (procedure-body object)
                     '<procedure-env>))
      (display object)))

(define the-global-environment (setup-environment))
(driver-loop)

; (install-quoted-package)
; (install-assignment-package
; (install-definition-package)
; (install-lambda-package)
; (install-if-package)
; (install-begin-package)
; (install-application-package)
; (install-cond-package)
; (install-or-package)
; (install-and-package)
