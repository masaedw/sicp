(add-load-path ".")
(use SICP.DDP)
(use SICP)

(define (deriv exp var)
  (cond ((number? exp) 0)
        ((variable? exp) (if (same-variable? exp var) 1 0))
        (else
         (let ((proc (get-method 'deriv (operator exp))))
           (proc (operands exp) var)))))

(define (operator exp) (car exp))

(define (operands exp) (cdr exp))

(define (sum exp var)
  (make-sum (deriv (addend exp) var)
            (deriv (augend exp) var)))

(define (product exp var)
  (make-sum
   (make-product (multiplier exp)
                 (deriv (multiplicand exp) var))
   (make-product (deriv (multiplier exp) var)
                 (multiplicand exp))))

(define (exponentation exp var)
  (make-product (make-product (exponent exp)
                              (make-exponentation (base exp)
                                                  (make-sum (exponent exp) -1)))
                (deriv (base exp) var)))

(define (install-deriv-package)
  (put-method 'deriv '+ sum)
  (put-method 'deriv '* product)
  (put-method 'deriv '** exponentation)
  'done)
(install-deriv-package)

(define (variable? x) (symbol? x))

(define (same-variable? v1 v2)
  (and (variable? v1) (variable? v2) (eq? v1 v2)))

(define (=number? exp num)
  (and (number? exp) (= exp num)))

(define (make-sum a1 a2)
  (cond ((=number? a1 0) a2)
        ((=number? a2 0) a1)
        ((and (number? a1) (number? a2)) (+ a1 a2))
        (else (list '+ a1 a2))))

(define (make-product m1 m2)
  (cond ((or (=number? m1 0) (=number? m2 0)) 0)
        ((=number? m1 1) m2)
        ((=number? m2 1) m1)
        ((and (number? m1) (number? m2)) (* m1 m2))
        (else (list '* m1 m2))))

(define (make-exponentation b e)
  (cond ((=number? e 0) 1)
        ((=number? e 1) b)
        ((=number? b 0) 0)
        ((=number? b 1) 1)
        ((and (number? b) (number? e)) (expt b e))
        (else (list '** b e))))

(define (addend s) (car s))

(define (augend s)
  (if (eq? (cddr s) '())
      (cadr s)
      (cons '+ (cdr s))))

(define (multiplier p) (car p))

(define (multiplicand p)
  (if (eq? (cddr p) '())
      (cadr p)
      (cons '* (cdr p))))

(define (base e) (car e))

(define (exponent e) (cadr e))

;; a. 手続き deriv 内の各記号の処理を分離し、加法的に微分手続きを追加で
;; きるようにした。
;;
;; number? や variable? がデータ主導の振り分けに吸収できないのは、それ
;; ぞれを表す型タグを付加できないから。
;; (中途半端)
;;

(use slib)
(require 'trace)
;;(trace deriv)
(define (main args)
  (p (deriv '(+ x 3) 'x)
     (deriv '(* x y z) 'x)
     (deriv '(* x y (+ x 3)) 'x)
     (deriv '(** x y) 'x)))