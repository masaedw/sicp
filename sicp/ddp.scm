(define-module sicp.ddp
  (use srfi-1)
  (use gauche.sequence)
  (export-all)
  ;; Data-Directed Programming package
  ;; SICP(J) P.99
  )

(select-module sicp.ddp)

(define (square x) (* x x))

(define *type-op-table* (make-hash-table 'equal?))

(define (put-method op type item)
  (hash-table-put! *type-op-table* (list op type) item))

(define (get-method op type)
  (if (hash-table-exists? *type-op-table* (list op type))
      (hash-table-get *type-op-table* (list op type))
      #f))

(define *coercion-table* (make-hash-table 'equal?))

(define (put-coercion from-type to-type proc)
  (hash-table-put! *coercion-table* (list from-type to-type) proc))

(define (get-coercion from-type to-type)
  (if (hash-table-exists? *coercion-table* (list from-type to-type))
      (hash-table-get *coercion-table* (list from-type to-type))
      #f))

(define (attach-tag type-tag contents)
  (if (number? contents)
    contents
    (cons type-tag contents)))

(define (type-tag datum)
  (cond [(number? datum) 'scheme-number]
        [(pair? datum)
         (car datum)]
        [else
         (error "Bad tagged datum -- TYPE-TAG" datum)]))

(define (is-a? datum type)
  (eq? (type-tag datum) type))

(define (contents datum)
  (cond [(number? datum) datum]
        [(pair? datum)
         (cdr datum)]
        [else
         (error "Bad tagged datum -- CONTENTS" datum)]))

;; 2.84 二つの型のいずれが塔の中で高いかをテストする方法
(define (higher t1 t2)
  (cond [(eq? t1 t2) #f]
        [(memq t2 (ancestors t1)) #t]
        [(memq t1 (ancestors t2)) #f]
        [else (error "Independent types" t1 t2)]))

(define (ancestors type)
  (let loop ([a ()]
             [t type])
    (let1 p (parent t)
      (if (not p)
        (reverse a)
        (loop (cons p a) p)))))

(define *parent-table* (make-hash-table 'equal?))

(define (parent type)
  (if (hash-table-exists? *parent-table* type)
    (hash-table-get *parent-table* type)
    #f))

(define (inherit child parent)
  (hash-table-put! *parent-table* child parent))

;; 2.84 apply-generic を raise を用いて書き直したもの
(define (apply-generic op . args)
  (define (highest types)
    (find-max types :compare higher))

  (define (coerce-to type n)
    (if (is-a? n type)
      n
      (coerce-to type (raise n))))

  (define (all-same xs)
    (every (^x (eq? x (car xs))) xs))

  (cond [(null? args)
         (error "No given args")]
        [(= 1 (length args))
         (let ([proc (get-method op (type-tag (car args)))])
           (if proc
             (proc (contents (car args)))
             (error "No method for that type" (list op (type-tag (car args))))))]
        [(< 1 (length args))
         (let ((type-tags (map type-tag args)))
           (let ((proc (get-method op type-tags)))
             (if proc
               (apply proc (map contents args))
               (if (not (all-same type-tags))
                 (let* ([highest-type (highest type-tags)]
                        [proc (get-method op (map (^x highest-type) type-tags))])
                   (apply proc (map (^x (coerce-to highest-type x)) args)))
                 (error "No method for these types"
                   (list op type-tags))))))]))


;; ジェネリック関数の定義
(define (add x y) (apply-generic 'add x y))

(define (sub x y) (apply-generic 'sub x y))

(define (mul x y) (apply-generic 'mul x y))

(define (div x y) (apply-generic 'div x y))

(define (equ? x y) (apply-generic 'equ? x y))

(define (=zero? x) (apply-generic '=zero? x))

(define (exp x y) (apply-generic 'exp x y))

(define (raise x) (apply-generic 'raise x))

;; 普通の数値パッケージ
(define (install-scheme-number-package)
  (define (tag x)
    (attach-tag 'scheme-number x))

  (define (scheme-number->complex n)
    (make-complex-from-real-imag (contents n) 0))

  (define (scheme-number->scheme-number n) n)

  (define (scheme-number->rational n)
    (make-rational (contents n) 1))

  (put-method 'add '(scheme-number scheme-number)
              (lambda (x y) (tag (+ x y))))
  (put-method 'sub '(scheme-number scheme-number)
              (lambda (x y) (tag (- x y))))
  (put-method 'mul '(scheme-number scheme-number)
              (lambda (x y) (tag (* x y))))
  (put-method 'div '(scheme-number scheme-number)
              (lambda (x y) (tag (/ x y))))
  (put-method 'equ? '(scheme-number scheme-number)
              (lambda (x y) (= x y)))
  (put-method '=zero? 'scheme-number
              (lambda (x) (= x 0)))
  (put-method 'make 'scheme-number
              (lambda (x) (tag x)))

  (put-method 'exp '(scheme-number scheme-number)
              (lambda (x y) (tag (expt x y))))

  (put-method 'raise 'scheme-number
              (^x (make-rational (contents x) 1)))

  (inherit 'scheme-number 'rational)

  (put-coercion 'scheme-number 'complex scheme-number->complex)
  (put-coercion 'scheme-number 'rational scheme-number->rational)
  (put-coercion 'scheme-number 'scheme-number scheme-number->scheme-number)

  'done)

(define (make-scheme-number n)
  ((get-method 'make 'scheme-number) n))


;; 有理数パッケージ
(define (install-rational-package)
  ;; 内部手続き
  (define (numer x) (car x))
  (define (denom x) (cdr x))
  (define (make-rat n d)
    (letrec ((g (gcd n d))
             (h (if (< d 0) (- g) g)))
      (cons (/ n h) (/ d h))))
  (define (add-rat x y)
    (make-rat (+ (* (numer x) (denom y))
                 (* (numer y) (denom x)))
              (* (denom x) (denom y))))
  (define (sub-rat x y)
    (make-rat (- (* (numer x) (denom y))
                 (* (numer y) (denom x)))
              (* (denom x) (denom y))))
  (define (mul-rat x y)
    (make-rat (* (numer x) (numer y))
              (* (denom x) (denom y))))
  (define (div-rat x y)
    (make-rat (* (numer x) (numer y))
              (* (denom x) (numer y))))
  (define (equ?-rat x y)
    (and (= (numer x) (numer y)) (= (denom x) (denom y))))
  (define (=zero?-rat x)
    (= (numer x) 0))

  (define (rational->complex n)
    (make-complex-from-real-imag (contents n) 0))

  (define (rational->rational n) n)

  ;; システムの他の部分へのインターフェース
  (define (tag x) (attach-tag 'rational x))
  (put-method 'add '(rational rational)
              (lambda (x y) (tag (add-rat x y))))
  (put-method 'sub '(rational rational)
              (lambda (x y) (tag (sub-rat x y))))
  (put-method 'mul '(rational rational)
              (lambda (x y) (tag (mul-rat x y))))
  (put-method 'div '(rational rational)
              (lambda (x y) (tag (div-rat x y))))
  (put-method 'equ? '(rational rational)
              equ?-rat)
  (put-method '=zero? 'rational
              =zero?-rat)

  (put-method 'make 'rational
              (lambda (n d) (tag (make-rat n d))))

  (put-method 'raise 'rational
              (^x (make-complex-from-real-imag (/ (numer x) (denom x)) 0)))

  (inherit 'rational 'complex)

  (put-coercion 'rational 'complex rational->complex)
  (put-coercion 'rational 'rational rational->rational)

  'done)


(define (make-rational n d)
  ((get-method 'make 'rational) n d))

;; 複素数パッケージ
;; 複素数の直交座標系による実装
(define (install-rectangular-package)
  ;; 内部手続き
  (define (real-part z) (car z))
  (define (imag-part z) (cdr z))
  (define (make-from-real-imag x y) (cons x y))
  (define (magnitude z)
    (sqrt (+ (square (real-part z))
             (square (imag-part z)))))
  (define (angle z)
    (atan (imag-part z) (real-part z)))
  (define (make-from-mag-ang r a)
    (cons (* r (cos a)) (* r (sin a))))

  ;; システムの他の部分とのインターフェース
  (define (tag x) (attach-tag 'rectangular x))
  (put-method 'real-part 'rectangular real-part)
  (put-method 'imag-part 'rectangular imag-part)
  (put-method 'magnitude 'rectangular magnitude)
  (put-method 'angle 'rectangular angle)
  (put-method 'make-from-real-imag 'rectangular
              (lambda (x y) (tag (make-from-real-imag x y))))
  (put-method 'make-from-mag-ang 'rectangular
              (lambda (r a) (tag (make-from-mag-ang r a))))
  'done)

;; 複素数の極座標系による実装
(define (install-polar-packge)
  ;; 内部手続き
  (define (magnitude z) (car z))
  (define (angle z) (cdr z))
  (define (make-from-mag-ang r a) (cons r a))
  (define (real-part z)
    (* (magnitude z) (cos (angle z))))
  (define (imag-part z)
    (* (magnitude z) (sin (angle z))))
  (define (make-from-real-imag x y)
    (cons (sqrt (+ (square x) (square y)))
          (atan y x)))

  ;; システムの他の部分とのインターフェイス
  (define (tag x) (attach-tag 'polar x))
  (put-method 'real-part 'polar real-part)
  (put-method 'imag-part 'polar imag-part)
  (put-method 'magnitude 'polar magnitude)
  (put-method 'angle 'polar angle)
  (put-method 'make-from-real-imag 'polar
              (lambda (x y) (tag (make-from-real-imag x y))))
  (put-method 'make-from-mag-ang 'polar
              (lambda (r a) (tag (make-from-mag-ang r a))))
  'done)

(define (real-part z) (apply-generic 'real-part z))
(define (imag-part z) (apply-generic 'imag-part z))
(define (magnitude z) (apply-generic 'magnitude z))
(define (angle z) (apply-generic 'angle z))

(define (make-from-real-imag x y)
  ((get-method 'make-from-real-imag 'rectangular) x y))
(define (make-from-mag-ang r a)
  ((get-method 'make-from-mag-ang 'polar) r a))

(define (install-complex-package)
  ;; 直交座標と極座標パッケージから取り入れた手続き
  (define (make-from-real-imag x y)
    ((get-method 'make-from-real-imag 'rectangular) x y))
  (define (make-from-mag-ang r a)
    ((get-method 'make-from-mag-ang 'polar) r a))

  ;; 内部手続き
  (define (add-complex z1 z2)
    (make-from-real-imag (+ (real-part z1) (real-part z2))
                         (+ (imag-part z1) (imag-part z2))))
  (define (sub-complex z1 z2)
    (make-from-real-imag (- (real-part z1) (real-part z2))
                         (- (imag-part z1) (imag-part z2))))
  (define (mul-complex z1 z2)
    (make-from-mag-ang (* (magnitude z1) (magnitude z2))
                       (+ (angle z1) (angle z1))))
  (define (add-complex z1 z2)
    (make-from-mag-ang (/ (magnitude z1) (magnitude z2))
                       (- (angle z1) (angle z1))))
  (define (equ?-complex z1 z2)
    (and (= (real-part z1) (real-part z2)) (= (imag-part z1) (imag-part z2))))
  (define (=zero?-complex z)
    (and (= (real-part z) 0) (= (imag-part z) 0)))

  (define (complex->complex z) z)

  ;; システムの他の部分へのインターフェース
  (define (tag z) (attach-tag 'complex z))
  (put-method 'add '(complex complex)
              (lambda (z1 z2) (tag (add-complex z1 z2))))
  (put-method 'sub '(complex complex)
              (lambda (z1 z2) (tag (sub-complex z1 z2))))
  (put-method 'mul '(complex complex)
              (lambda (z1 z2) (tag (mul-complex z1 z2))))
  (put-method 'div '(complex complex)
              (lambda (z1 z2) (tag (div-complex z1 z2))))
  (put-method 'equ? '(complex complex)
              equ?-complex)
  (put-method '=zero? 'complex
              =zero?-complex)
  (put-method 'make-from-real-imag 'complex
              (lambda (x y) (tag (make-from-real-imag x y))))
  (put-method 'make-from-mag-ang 'complex
              (lambda (r a) (tag (make-from-mag-ang r a))))
  (put-method 'real-part '(complex) real-part)
  (put-method 'imag-part '(complex) imag-part)
  (put-method 'magnitude '(complex) magnitude)
  (put-method 'angle '(complex) angle)
  (put-coercion 'complex 'complex complex->complex)
  'done)

(define (make-complex-from-real-imag x y)
  ((get-method 'make-from-real-imag 'complex) x y))

(define (make-complex-from-mag-ang r a)
  ((get-method 'make-from-mag-ang 'complex) r a))

(install-scheme-number-package)
(install-rational-package)
(install-rectangular-package)
(install-polar-packge)
(install-complex-package)


(provide "sicp/ddp")
