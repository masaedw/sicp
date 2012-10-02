(define (f n)
  (define (f-iter a b c n)
    (if (= n 0)
      c
      (f-iter (+ a (* 2 b) (* 3 c)) a b (- n 1))))
  (f-iter 2 1 0 n))
