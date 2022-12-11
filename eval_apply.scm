;:
;: eval-loop
;:

(load "environment.scm")
(load "grammars.scm")
(load "debug_print.scm")

(define (eval expr env)
    (cond ((self-evaluating? expr) expr)
          ((variable? expr) (lookup-variable-value expr env))
          (else ((get 'eval (car expr)) expr env))))

(define (eval expr env)
  (display-debug "eval: ")
  (display-debug expr)
  ;(display-debug "pair?: " (application? expr))
  (newline-debug)
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
         ;(display-debug "lv: " (list-of-values (operands expr) env))
         ;(newline-debug)
         (apply (eval (operator expr) env)
                (list-of-values (operands expr) env)))
        (else
            (error-report "Unknown exprression type -- EVAL" expr))))

(define (apply procedure arguments)
    (cond ((primitive-procedure? procedure)
                (apply-primitive-procedure procedure arguments))
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
                        arguments
                        (procedure-environment procedure))))
          (else (error-report "Unknown procedure type -- APPLY" procedure))))



(define input-prompt ";;; M-Eval input:")
(define output-prompt ";;; M-Eval value:")

(define (driver-loop)
  (prompt-for-input input-prompt)
  (let ((input (read)))
    (let ((output (eval input the-global-environment)))
      (announce-output output-prompt)
      (user-print-output output)))
  (driver-loop))


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
