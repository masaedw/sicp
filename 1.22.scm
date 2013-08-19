(load "1.21.scm")
(use gauche.time) ;; gauche には runtime 手続はない

(define (prime? n)
  (= n (smallest-divisor n)))

(define (timed-prime-test n)
  (print n)
  (let1 counter (make <user-time-counter>)
    (if (with-time-counter counter
          (prime? n))
      (begin
        (print " *** ")
        (print (time-counter-value counter))))))

;; 指定範囲の連続する奇整数について素数性を調べる
(define (search-for-primes start end)
  (cond ((> start end)
         ;; do nothing
         )
        ((odd? start)
         (search-for-primes (+ 1 start) end))
        (else
         (timed-prime-test start)
         (search-for-primes (+ 2 start) end))))

(define (main args)
  )
