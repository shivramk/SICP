(define (make-point x y) (cons x y))
(define (x-point pt) (car pt))
(define (y-point pt) (cdr pt))

(define (print-point p)
  (display "(")
  (display (x-point p))
  (display ", ")
  (display (y-point p))
  (display ")")
  (newline))

; Createa retangle given two opposite points
(define (make-rectangle pt1 pt2)
  (cons pt1 pt2))

(define (sidex rect)
  (abs (- (x-point (car rect)) (x-point (cdr rect)))))

(define (sidey rect)
  (abs (- (y-point (car rect)) (y-point (cdr rect)))))

(define (area rect)
  (* (sidex rect) (sidey rect)))

(define (perimeter rect)
  (* 2 (+ (sidex rect) (sidey rect))))

(define r2 (make-rectangle
	    (make-point 2 3)
	    (make-point 4 8)))
(area r2); 10
(perimeter r2); 14

; Define rectangle using two sides
(define (make-rectangle sx sy)
  (cons sx sy))

(define (sidex rect) (car rect))
(define (sidey rect) (cdr rect))

(define r2 (make-rectangle 2 5))
(area r2) ; 10
(perimeter r2) ; 14
