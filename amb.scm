#;(use gauche.parameter)

#;(define *amb-cont* #f)

#;(define (amb . args)
  (if (null? args)
    (if *amb-cont*
      (*amb-cont* #f)
      (error "no amb"))
    (call/cc (lambda (yield)
               (let1 pre *amb-cont*
                 (for-each (lambda (x)
                             (call/cc (lambda (break)
                                        (set! *amb-cont* break)
                                        (yield x))))
                           args)
                 (set! *amb-cont* pre)
                 (amb))))
    ))

(define (distinct? items)
  (cond ((null? items) #t)
        ((null? (cdr items)) #t)
        ((member (car items) (cdr items)) #f)
        (else (distinct? (cdr items)))))

#;(define (multiple-dwelling)
  (let ((baker (amb 1 2 3 4 5))
        (cooper (amb 1 2 3 4 5))
        (fletcher (amb 1 2 3 4 5))
        (miller (amb 1 2 3 4 5))
        (smith (amb 1 2 3 4 5)))
    (require (distinct? (list baker cooper fletcher miller smith)))
    `((baker ,baker)
      (cooper ,cooper)
      (fletcher ,fletcher)
      (miller ,miller)
      (smith ,smith))))

#;(define fail #f)
#;(define-syntax amb
  (syntax-rules ()
    ((_) (fail))
    ((_ a) a)
    ((_ a b ...)
     (let ((fail0 fail))
       (call/cc
	(lambda (cc)
	  (set! fail
		(lambda ()
		  (set! fail fail0)
		  (cc (amb b ...))))
	  (cc a)))))))

;; stack of cc.
(define fail '())

;;; nondeterminsm macro operator
(define-syntax amb
  (syntax-rules ()
    ((_) ((pop! fail)))
    ((_ a) a)
    ((_ a b ...)
     (call/cc
      (lambda (cc)
        (push! fail (lambda ()
                      (cc (amb b ...))))
        a)))))

(define (require p)
  (if (not p) (amb)))
