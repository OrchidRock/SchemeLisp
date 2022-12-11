;:
;: Testing for Primality
;:

;: time/space complexity: ThTa(sqrt(n))
;: when n = 1000000005721, elapsed-time = [3.39~3.47]
(define (smallest-divisor n)
    (define (divides? a b)
        (= (remainder b a) 0))
    (define (find-divisor n test-divisor)
        (cond ((> (square test-divisor) n) n)
              ((divides? test-divisor n) test-divisor)
		      (else (find-divisor n (+ test-divisor 1)))))
    (find-divisor n 2))

;: To devise another smallest-divisor version
;: to skip lots of neddless test.
;: when n = 1000000005721, elapsed-time=[2.19~2.3]
;:
(define (smallest-divisor2 n)
    (define (divides? a b)
        (= (remainder b a) 0))
    (define (next test-divisor)
        (if (= test-divisor 2)
            3
            (+ test-divisor 2)))
    (define (find-divisor n test-divisor)
        (cond ((> (square test-divisor) n) n)
              ((divides? test-divisor n) test-divisor)
              (else (find-divisor n (next test-divisor)))))
    (find-divisor n 2))

;: remove lots of square process.
;: But it's not work to improve the efficiency of process.
;:
(define (smallest-divisor3 n)
    (define (divides? a b)
        (= (remainder b a) 0))
    (define (next test-divisor)
        (if (= test-divisor 2)
            3
            (+ test-divisor 2)))
    (define (find-divisor n test-divisor sqrt-n)
        (cond ((> test-divisor sqrt-n) n)
              ((divides? test-divisor n) test-divisor)
              (else (find-divisor n (next test-divisor) sqrt-n))))
    (find-divisor n 2 (sqrt n)))


(define (prime? n)
    (if (< n 2)
        false
        ;(= n (smallest-divisor n))))
        (= n (smallest-divisor2 n))))
        ;(= n (smallest-divisor3 n))))

;: The format test
;: Probabilistic algorithm. complexity: ThTa(log(n))
;:

;: complute a^n (mod m)
;: this process familiar with fast-expt process.
(define (expmod base exp m)
    (define (miller-rabin-test? a n)
        (and (> a 1)
             (< a (- n 1))
             (= (remainder (square a) n) 1)))

    (define (recursive-body base exp m)
        (cond ((= exp 0) 1)
              ((even? exp) (remainder (square (recursive-body base (/ exp 2) m)) m))
              (else (remainder (* base (recursive-body base (- exp 1) m)) m))))

    (if (miller-rabin-test? base m)
        0
        (recursive-body base exp m)))

;: this procedure will be very slow when a and n both are large number.
;:
(load "fast_expt.scm")
(define (expmod2 base exp m)
    (remainder (fast-expt-iter base exp) m))

;: complexity: ThTa(n)
;: this procedure will be very slow when a and n both are large number.
;:
(define (expmod3 base exp m)
    (cond ((= exp 0) 1)
          ((even? exp) (remainder (* (expmod3 base (/ exp 2) m)
                                     (expmod3 base (/ exp 2) m))
                                  m))
          (else (remainder (* base (expmod3 base (- exp 1) m))
                           m))))

;:
;:
(define (fermat-test n)
    (define (try-it a)
        (= (expmod a n n) a))
        ;(= (expmod2 a n n) a))
        ;(= (expmod3 a n n) a))
    (try-it (+ 1 (random (- n 1)))))

;: Miller Rabin test won't be fooled by carmichael numbers.
;;
(define (fermat-test-mr n)
    (define (iter a n)
        (cond ((= a n) true)
              ((= (expmod a (- n 1) n) 1) (iter (+ a 1) n))
              (else false)))
    (iter 1 n))


;: By running this test with more and more randomly chosen values of a we can
;: make the probabilistic of error as samll as we like.
(define (fast-prime? n times)
    (cond ((= times 0) true)
          ((< n 2) false)
          ((fermat-test n) (fast-prime? n (- times 1)))
          (else false)))

;: There do exist numbers fool the Fermat test:
;: Carmichael number: 561, 1105, 1729, 2465, 2821, 6601
(define (carmichael-test? n)
    (define (iter a n) ; a>0 and a<n
        (cond ((= a n) true)
              ((= (expmod a n n) a) (iter (+ a 1) n))
              (else false)))
    (iter 1 n))

;: time cost test
;: test instance: 1000000005721
(define (timed-prime-test n)
	(define (report-prime elapsed-time)
	  	(display "***")
		(display elapsed-time))
	(define (start-prime-test n start-time)
	    ;(if (prime? n)
        (if (fast-prime? n 100)
		    (report-prime (- (runtime) start-time))
		    (report-prime (- (runtime) start-time))))
	(newline)
	(display n)
	(start-prime-test n (runtime)))

(define (file-op-demo filename seq)
    (let ((port (open-output-file filename)))
        (define (iter seq)
            (if (not (null? seq))
                (begin (write-line (car seq) port)
                       (iter (cdr seq)))))
        (iter seq)
        (flush-output port)
        (close-port port)))

(define (search-for-primes a b)
    (define (report-prime n)
	    (display n)
	    (display " "))
    (define (iter a b result)
  	(if (prime? a)
	    (begin (report-prime a)
                   (if (< a b)
                        (iter (+ a 1) b (append result (list a)))
                        (file-op-demo "primes_table.txt" result)))
	    (if (>= a b)
	  	(begin (display " END")
                        (file-op-demo "primes_table.txt" result))
		(iter (+ a 1) b result))))
    (iter a b '()))
