
(declare (usual-integrations))

;: liner recursive process

(define (fast-expt b n)
    (define (square x)(* x x))
    (cond ((< n 0) -1)
          ((= n 0) 1)
          ((even? n) (square (fast-expt b (/ n 2))))
          (else (* b (fast-expt b (- n 1))))))

;: exersice 1.16
;: interative process 
;:
(define (fast-expt-iter b n)
    ;(define (square x)(* x x))
    (define (iter b n a)
        (cond ((= n 0) a)
              ((even? n) (iter (square b) (/ n 2) a))
	          (else (iter b (- n 1) (* a b))))) 

    (cond ((< n 0) -1)
          ((= n 0) 1)
          (else (iter b n 1))))

