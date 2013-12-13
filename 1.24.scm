(load "./1.23.scm")

(define (expmod base exp m)
  (cond ((= exp 0) 1)
        ((even? exp)
         (remainder (square (expmod base (/ exp 2) m))
                    m))
        (else
         (remainder (* base (expmod base (- exp 1) m))
                    m))))

(define (fermat-test n)
  (define (try-it a)
    (= (expmod a n n) a))
  (try-it (+ 1 (random (- n 1)))))

(define (fast-prime? n times)
  (cond ((= times 0) true)
        ((fermat-test n) (fast-prime? n (-times 1)))
        (else false)))

(define (prime-test)
  (prime? 1009)
  (prime? 1013)
  (prime? 1019)

  (prime? 10007)
  (prime? 10009)
  (prime? 10037)

  (prime? 100003)
  (prime? 100019)
  (prime? 100043)

  (prime? 1000033)
  (prime? 1000037)
  (prime? 1000039))
