(load "./2.2-segment.scm")

(define (square x) (* x x))

(define (length-segment s)
  (let ((start (start-segment s))
        (end (end-segment s)))
    (sqrt (+ (square (- (x-point start) (x-point end)))
             (square (- (y-point start) (y-point end)))))))

;; 底辺と高さバージョン
(define (make-rectangle bl h) (cons bl h))
(define (width-rectangle r)
  (length-segment (car r)))
(define (height-rectangle r) (cdr r))

;; 対角線と底辺のx軸からの角度バージョン


(define (make-rectangle bl h)

(define (perimeter-rectangle r)
  (* 2 (+ (width-rectangle r)
          (height-rectangle r))))

(define (area-rectangle r)
  (* (width-rectangle r)
     (height-rectangle r)))

(define (main args)
  (define rect (make-rectangle (make-segment (make-point 0 0)
                                             (make-point 0 5))
                               5))
  (print (perimeter-rectangle rect))
  (print (area-rectangle rect)))