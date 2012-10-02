;; -*- coding: utf-8 -*-

(add-load-path ".")
(use SICP)

(define (make-point x y) (list x y))
(define (x-of p) (car p))
(define (y-of p) (cadr p))

(define empty-board '())

(define (adjoin-position new-row k rest-of-queens)
  (cons (make-point (+ 1 (length rest-of-queens))
                    new-row)
        rest-of-queens))

(define (safe-pair? a b)
  (not (or (= (x-of a) (x-of b)) ;; 横
           (= (y-of a) (y-of b)) ;; 縦
           (= (- (x-of a) (y-of a)) ;; 斜め
              (- (x-of b) (y-of b)))
           (= (+ (x-of a) (y-of a))
              (+ (x-of b) (y-of b))))))

(define (safe? k positions)
  (define (safe?-iter position positions)
    (cond ((null? positions) #t)
          ((safe-pair? position (car positions))
           (safe?-iter position (cdr positions)))
          (else #f)))
  (safe?-iter (car positions) (cdr positions)))

(define (queens board-size)
  (define (queen-cols k)
    (if (= k 0)
        (list empty-board)
        (filter
         (lambda (positions) (safe? k positions))
         (flatmap
          (lambda (rest-of-queens)
            (map (lambda (new-row)
                   (adjoin-position new-row k rest-of-queens))
                 (enumerate-interval 1 board-size)))
          (queen-cols (- k 1))))))
  (queen-cols board-size))

(define (board-output board n)
  (display n)
  (display " ")
  (display (length board))
  (newline)
  (for-each (lambda (l)
              (for-each (lambda (i)
                          (display (cadr i))
                          (display " "))
                        (reverse l))
              (newline))
            board))

(define (main args)
  (board-output (queens 8) 8))
