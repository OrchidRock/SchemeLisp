;:

(define (an-integer-between low high)
    (if (>= low high)
        high
        (amb low (an-integer-between (+ low 1) high))))

(define (an-integer-between low high)
    (require (<= low high))
    (amb low (an-integer-between (+ low 1) high)))

(define (a-pythagorean-triple-between low high)
  (let ((i (an-integer-between low high)))
    (let ((j (an-integer-between i high)))
      (let ((k (an-integer-between j high)))
        (require (= (+ (* i i) (* j j)) (* k k)))
        (list i j k)))))


(a-pythagorean-triple-between 3 30)

