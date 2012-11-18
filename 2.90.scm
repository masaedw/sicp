(add-load-path ".")
(use sicp.ddp)
(use gauche.test)

;; 過去の実装を壊してないこと
(load "2.88")

(test-start "2.90")

(test* "(make-from-term-list 'x '((5 2) (3 1) (0 4)))"
       '(sparse x (5 2) (3 1) (0 4))
       (make-from-term-list 'x '((5 2) (3 1) (0 4)))
       )

(test* "variable of (sparse x (5 2) (3 1) (0 4))"
       'x
       (variable (make-from-term-list 'x '((5 2) (3 1) (0 4))))
       )

(test* "first-term of (sparse x (5 2) (3 1) (0 4))"
       '(5 2)
       (first-term (make-from-term-list 'x '((5 2) (3 1) (0 4))))
       )

(test* "rest-terms of (sparse x (5 2) (3 1) (0 4))"
       (make-from-term-list 'x '((3 1) (0 4)))
       (rest-terms (make-from-term-list 'x '((5 2) (3 1) (0 4))))
       )

(test* "adjoin-term of (5 2) (sparse x (3 1) (0 4))"
       (make-from-term-list 'x '((5 2) (3 1) (0 4)))
       (adjoin-term (make-term 5 2) (make-from-term-list 'x '((3 1) (0 4))))
       )

(test* "adjoin-term of (2 2) (sparse x (3 1) (0 4))"
       (make-from-term-list 'x '((3 1) (2 2) (0 4)))
       (adjoin-term (make-term 2 2) (make-from-term-list 'x '((3 1) (0 4))))
       )
