(load "./1.21.scm")
(use gauche.time) ;; gauche には runtime 手続はない

(define (prime? n)
  (= n (smallest-divisor n)))

(define (timed-prime-test n)
  (display n)
  (let1 counter (make <user-time-counter>)
    (if (with-time-counter counter
          (prime? n))
      (begin
        (display " *** ")
        (display (time-counter-value counter)))))
  (newline))

;; 指定範囲の連続する奇整数について素数性を調べる
(define (search-for-primes start end)
  (cond ((> start end)
         ;; do nothing
         )
        ((even? start)
         (search-for-primes (+ 1 start) end))
        (else
         (timed-prime-test start)
         (search-for-primes (+ 2 start) end))))

(define (find-primes)
  (search-for-primes 1000 (+ 1000 50))
  ;; → 1009 1013 1019
  (search-for-primes 10000 (+ 10000 50))
  ;; → 10007 10009 100037
  (search-for-primes 100000 (+ 100000 50))
  ;; → 100003 100019 100043
  (search-for-primes 1000000 (+ 1000000 50))
  ;; → 1000033 1000037 1000039
  )

(define (silentprimes n)
  (with-output-to-file "/dev/null"
    (^() (dotimes (_ 100) (search-for-primes n (+ n 100))))))


(define (main args)
  (time (silentprimes 1000))
  ;; user 0.110
  (time (silentprimes 10000))
  ;; user 0.140
  (time (silentprimes 100000))
  ;; user 0.180
  (time (silentprimes 1000000))
  ;; user 0.330
  (time (silentprimes 10000000))
  ;; user 0.450
  (time (silentprimes 100000000))
  ;; user 2.200
  )

;; 必要なステップ数に比例した時間で走ってるんだろうか・・？
;; 最後のやつを除けばそれっぽくはあるが、よくわからない
