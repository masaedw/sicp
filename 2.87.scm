(add-load-path ".")
(use sicp.ddp)
(use gauche.test)

;; 過去の実装を壊してないこと
(load "2.85")

(test-start "2.87")

(test* "add polynomials"
       (make-polynomial 'x '((3 5) (2 8) (1 1) (0 2)))
       (add (make-polynomial 'x '((2 1) (1 1) (0 1)))
            (make-polynomial 'x '((3 5) (2 7) (0 1))))
       equ?)

(test* "add polynomials with other numeric types"
       (make-polynomial 'x `((3 5) (2 ,(make-rational 18 5)) (1 1) (0 2)))
       (add (make-polynomial 'x `((2 ,(make-rational 3 5)) (1 1) (0 1)))
            (make-polynomial 'x '((3 5) (2 3) (0 1))))
       equ?)


(test* "mul polynomials"
       (make-polynomial 'x '((5 5) (4 5) (2 1) (1 1)))
       (mul (make-polynomial 'x '((2 1) (1 1)))
            (make-polynomial 'x '((3 5) (0 1))))
       equ?)

;; ここまで、テキストの実装が正しく行えていることの確認

(define (test-multiple name . xs)
  (define (test-sub arg)
    (test* name
           (car arg)
           (cadr arg)))
  (map test-sub (slices xs 2)))

(test-multiple "=zero?"
               #t (=zero? (make-polynomial 'x '((5 0) (4 0) (3 0))))
               #f (=zero? (make-polynomial 'x '((5 0) (4 1) (3 0))))
               #t (=zero? (make-polynomial 'y `((5 0) (4 ,(make-polynomial 'x '((5 0) (4 0) (3 0)))))))
               )
