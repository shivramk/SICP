#lang planet neil/sicp

(define (make-interval a b) (cons a b))
(define (upper-bound interval) (cdr interval))
(define (lower-bound interval) (car interval))

(define (print-interval i)
  (display "(")
  (display (lower-bound i))
  (display ",")
  (display (upper-bound i))
  (display ")")
  (newline))

(define (width i)
  (/ (- (upper-bound i) (lower-bound i)) 2))

(define (add-interval x y)
  (make-interval (+ (lower-bound x) (lower-bound y))
                 (+ (upper-bound x) (upper-bound y))))

(define (mul-interval x y)
  (let ((p1 (* (lower-bound x) (lower-bound y)))
        (p2 (* (lower-bound x) (upper-bound y)))
        (p3 (* (upper-bound x) (lower-bound y)))
        (p4 (* (upper-bound x) (upper-bound y))))
    (make-interval (min p1 p2 p3 p4)
                   (max p1 p2 p3 p4))))

(define (div-interval x y)
  (if (= (width y) 0)
      (error "Can't divide by an interval that spans zero")
      (mul-interval
       x
       (make-interval (/ 1.0 (upper-bound y))
                      (/ 1.0 (lower-bound y))))))

(define (make-center-percent c p)
  (let ((diff (* c (/ p 100.0))))
    (make-interval (- c diff) (+ c diff))))

(define (center i)
  (/ (+ (lower-bound i) (upper-bound i)) 2))

(define (percent i)
  (* 100.0 (/ (- (center i) (lower-bound i)) (center i))))

(define (par1 r1 r2)
  (div-interval (mul-interval r1 r2)
                (add-interval r1 r2)))

(define (par2 r1 r2)
  (let ((one (make-interval 1 1)))
    (div-interval
     one (add-interval (div-interval one r1)
                       (div-interval one r2)))))

(define r1 (make-center-percent 10 5))
(define r2 (make-center-percent 20 5))

; Lem is right.. The following 2 expressions do give different results
(print-interval (par1 r1 r2))
(print-interval (par2 r1 r2))

(define i1 (make-center-percent 10 5))
(define i2 (make-center-percent 5 10))
(define i3 (div-interval i1 i1))
(define i4 (div-interval i1 i2))

; These give the same result
(percent i3)
(percent i4)

; Aha! Insight.
(percent (par2 r1 r2)) ; 5%
(percent (par1 r1 r2)) ; 15%

(percent (add-interval i1 i2))