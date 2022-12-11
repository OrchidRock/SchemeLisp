

(define DEBUG false)

(define (newline-debug)
    (if (eq? DEBUG true)
        (newline)
        ))

(define (display-debug obj)
    (if (eq? DEBUG true)
        (display obj)))

(define (error-report str expr)
    (display str)
    (display " -- ")
    (display expr)
    (newline))

(define (prompt-for-input string)
  (newline)
  (newline)
  (display string)
  (newline))

(define (announce-output string)
  (newline)
  (display string)
  (newline))

(define (user-print-objects objects)
    (cond ((pair? objects)
               (user-print (car objects))
               (display-debug " ")
               (user-print-objects (cdr objects)))
          ((null? objects) 'ok)
          (else (user-print objects))))

(define (user-print object)
  (if (compound-procedure? object)
      (display-debug (list 'compound-procedure
                     (procedure-parameters object)
                     (procedure-body object)
                     '<procedure-env>))
      (display-debug object)))

(define (user-print-output object)
    (if (compound-procedure? object)
      (display (list 'compound-procedure
                     (procedure-parameters object)
                     (procedure-body object)
                     '<procedure-env>))
      (display object)))

