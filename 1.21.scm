(define (square n)
  (* n n))

(define (smallest-divisor n)
  (find-divisor n 2))

(define (find-divisor n test-divisor)
  (cond ((> (square test-divisor) n) n)
        ((divides? test-divisor n) test-divisor)
        (else (find-divisor n (+ test-divisor 1)))))

(define (divides? a b)
  (= (remainder b a) 0))

(define (main args)
  (print #`"The smallest divisor of 199 is ,(smallest-divisor 199)")
  (print #`"The smallest divisor of 1999 is ,(smallest-divisor 1999)")
  (print #`"The smallest divisor of 19999 is ,(smallest-divisor 19999)"))
