(add-load-path ".")
(use sicp)

(define (reverser sequence)
  (fold-right (lambda (x y) (append y (list x))) nil sequence))

(define (reversel sequence)
  (fold-left (lambda (x y) (cons y x)) nil sequence))

(define (main args)
  (p (reverser '(1 2 3 4 5 6))
     (reversel '(1 2 3 4 5 6))))

;; right
;; (1 ? (2 ? (3 ? nil)))
;;
;; left
;; (((nil ? 1) ? 2) ? 3)
