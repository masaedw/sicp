(define (make-mobile left right)
  (list left right))

(define (make-branch length structure)
  (list length structure))

(define (left-branch mobile)
  (car mobile))

(define (right-branch mobile)
  (cadr mobile))

(define (branch-length branch)
  (car branch))

(define (branch-structure branch)
  (cadr branch))


(define (weight-of branch)
  (let ((structure (branch-structure branch)))
    (if (pair? structure) (total-weight structure) structure)))

(define (total-weight mobile)
  (+ (weight-of (left-branch mobile))
     (weight-of (right-branch mobile))))

(define (moment-of branch)
  (* (branch-length branch) (weight-of branch)))

(define (balanced? mobile)
  (if (not (pair? mobile))
      #t
      (and (= (moment-of (left-branch mobile))
              (moment-of (right-branch mobile)))
           (balanced? (branch-structure (left-branch mobile)))
           (balanced? (branch-structure (right-branch mobile))))))


(define (main args)
  (let* ((a (make-mobile (make-branch 5 2)
                         (make-branch 2 5)))
         (c (make-mobile (make-branch 5 a)
                         (make-branch 5 a))))
    (print (balanced? c))))