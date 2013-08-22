(load "./1.22.scm")

(define (next n)
  (if (= n 2)
    3
    (+ n 2)))

(define (prime-orig? n)
  (= n (smallest-divisor-orig n)))

(define (smallest-divisor-orig n)
  (find-divisor-orig n 2))

(define (find-divisor-orig n test-divisor)
  (cond ((> (square test-divisor) n) n)
        ((divides? test-divisor n) test-divisor)
        (else (find-divisor-orig n (+ test-divisor 1)))))
(define (prime? n)
  (= n (smallest-divisor n)))

(define (smallest-divisor n)
  (find-divisor n 2))

(define (find-divisor n test-divisor)
  (cond ((> (square test-divisor) n) n)
        ((divides? test-divisor n) test-divisor)
        (else (find-divisor n (next test-divisor)))))

(define (prime-test-orig)
  (prime-orig? 1009)
  (prime-orig? 1013)
  (prime-orig? 1019)
  (prime-orig? 10007)
  (prime-orig? 10009)
  (prime-orig? 10037)
  (prime-orig? 100003)
  (prime-orig? 100019)
  (prime-orig? 100043)
  (prime-orig? 1000033)
  (prime-orig? 1000037)
  (prime-orig? 1000039))

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

(define (main args)
  (time (dotimes (_ 1000) (prime-test-orig)))
  ;; user 1.410
  (time (dotimes (_ 1000) (prime-test)))
  ;; user 0.860
  )

;; だいぶ早くなったけど二倍速くはなっていない。
;; 60%ぐらいの速度になった。
;; 実行時間の中に占める素数判定でない部分、
;; つまり関数呼び出しや繰り返しのような部分が足をひっぱる要因になっているのではないか？
