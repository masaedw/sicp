(define-module sicp.ddp
  (export-all))
;; Data-Directed Programming package
;; SICP(J) P.99

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
  (cons type-tag contents))

(define (type-tag datum)
  (if (pair? datum)
      (car datum)
      (error "Bad tagged datum -- TYPE-TAG" datum)))

(define (contents datum)
  (if (pair? datum)
      (cdr datum)
      (error "Bad tagged datum -- CONTENTS" datum)))

(define (apply-generic op . args)
  (let ((type-tags (map type-tag args)))
    (let ((proc (get-method op type-tags)))
      (if proc
        (apply proc (map contents args))
        (if (= (length args) 2)
          (let ([type1 (car type-tags)]
                [type2 (cadr type-tags)]
                [a1 (car args)]
                [a2 (cadr args)])
            (if (eq? type1 type2)
              (error "No method for these types" (list op type-tags))
              (begin
                (let ([t1->t2 (get-coercion type1 type2)]
                      [t2->t1 (get-coercion type2 type1)])
                  (cond [t1->t2
                         (apply-generic op (t1->t2 a1) a2)]
                        [t2->t1
                         (apply-generic op a1 (t2->t1 a2))]
                        [else
                         (error "No method for these types"
                           (list op type-tags))])))))
          (error "No method for these types"
            (list op type-tags)))))))

(provide "sicp/ddp")
