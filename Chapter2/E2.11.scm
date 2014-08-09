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

; If you consider the signs of the upper and lower bounds of the two arguments
; there are 16 possible cases. 7 of which are invalid because u1 > l1 & u2 > l2
;
;  u1  l1  u2  l2  u1*l2 u2*l1 u1*u2 l1*l2      min            max
;  +   +   +   +     +     +     +     +       l1*l2          u1*u2
;  +   +   +   -     -     +     +     -       u1*l2          u1*u2
;  +   +   -   -     -     -     -     -       u1*l2          u2*l1
;  +   -   +   +     +     -     +     -       u2*l1          u1*u2
;  +   -   +   -     -     -     +     +   min(u1*l2, u2*l1)  u1*u2
;  +   -   -   -     -     +     -     +       u1*l2          l1*l2
;  -   -   +   +     -     -     -     -       u2*l1          u1*l2
;  -   -   +   -     +     -     -     +       u2*l1          l1*l2
;  -   -   -   -     +     +     +     +       l1*l2          u1*u2
;  +   +   -   +   (invalid)
;  +   -   -   +   (invalid)
;  -   +   +   +   (invalid)
;  -   +   +   -   (invalid)
;  -   +   -   +   (invalid)
;  -   +   -   -   (invalid)
;  -   -   -   +   (invalid)

(define (mul-interval x y)
  (let ((u1 (upper-bound x))
        (u2 (upper-bound y))
        (l1 (lower-bound x))
        (l2 (lower-bound y)))
    (let ((nu1 (< u1 0)) 
          (pu1 (>= u1 0))            
          (nu2 (< u2 0)) 
          (pu2 (>= u2 0))
          (nl1 (< l1 0))
          (pl1 (>= l1 0))
          (nl2 (< l2 0))
          (pl2 (>= l2 0)))
      (cond ((and pu1 pl1 pu2 pl2)
             (make-interval (* l1 l2) (* u1 u2)))
            ((and pu1 pl1 pu2 nl2)
             (make-interval (* u1 l2) (* u1 u2)))
            ((and pu1 pl1 nu2 nl2)
             (make-interval (* u1 l2) (* u2 l1)))
            ((and pu1 nl1 pu2 pl2)
             (make-interval (* u2 l1) (* u1 u2)))
            ((and pu1 nl1 nu2 nl2)
             (make-interval (* u1 l2) (* l1 l2)))
            ((and nu1 nl1 pu2 pl2)
             (make-interval (* u2 l1) (* u1 l2)))
            ((and nu1 nl1 pu2 nl2)
             (make-interval (* u2 l1) (* l1 l2)))
            ((and nu1 nl2 nu2 nl2)
             (make-interval (* l1 l2) (* u1 u2)))
            (else
             (make-interval (min (* u1 l2) (* l2 u1)) (* u1 u2)))))))

; Alyssa's original version
(define (mul-interval-orig x y)
  (let ((p1 (* (lower-bound x) (lower-bound y)))
        (p2 (* (lower-bound x) (upper-bound y)))
        (p3 (* (upper-bound x) (lower-bound y)))
        (p4 (* (upper-bound x) (upper-bound y))))
    (make-interval (min p1 p2 p3 p4)
                   (max p1 p2 p3 p4))))

(define i1 (make-interval 2 3))
(define i2 (make-interval -2 3))
(define i3 (make-interval -3 -2))

(define i4 (make-interval 4 5))
(define i5 (make-interval -4 5))
(define i6 (make-interval -5 -4))

; Check equality of intervals
(define (interval-eq? x y)
  (and (= (upper-bound x) (upper-bound y))
       (= (lower-bound x) (lower-bound y))))

(define (check-mul x y)
  (let ((m1 (mul-interval x y))
        (m2 (mul-interval-orig x y)))
    (if (interval-eq? m1 m2)
        (print-interval m1)
        (error "Multiply interval results dont match for" x y m1 m2))))

(check-mul i1 i4)
(check-mul i1 i5)
(check-mul i1 i6)

(check-mul i2 i4)
(check-mul i2 i5)
(check-mul i2 i6)

(check-mul i3 i4)
(check-mul i3 i5)
(check-mul i4 i6)