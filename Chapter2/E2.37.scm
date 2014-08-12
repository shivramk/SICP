#lang scheme

(define (accumulate op initial sequence)
  (if (null? sequence)
      initial
      (op (car sequence)
          (accumulate op initial (cdr sequence)))))

(define (accumulate-n op init seqs)
  (if (null? (car seqs))
      '()
      (cons (accumulate op init (map car seqs))
            (accumulate-n op init (map cdr seqs)))))

(define (dot-product v w)
  (accumulate + 0 (map * v w)))

(define v1 (list 1 2 3))
(define v2 (list 4 5 6))

(dot-product v1 v2) ; 32

(define (matrix-*-vector m v) 
  (map (lambda (x) (dot-product v x)) m))

; identity matrix
; 1 0 0
; 0 1 0
; 0 0 1
(define m1 (list (list 1 0 0) (list 0 1 0) (list 0 0 1)))
(define m2 (list (list 1 2 3) (list 4 5 6) (list 7 8 9)))

; should give v1
(matrix-*-vector m1 v1)
; should give (14, 32, 50)
(matrix-*-vector m2 v1)

(define (transpose mat) 
  (accumulate-n cons '() mat))

; should give m1
(transpose m1)
; should give 
; 1 4 7
; 2 5 8
; 3 6 9
(transpose m2)

(define (matrix-*-matrix m n)
  (let ((cols (transpose n)))
    (map (lambda (x) (map (lambda (y) (dot-product y x)) cols)) m)))

; should give m2
(matrix-*-matrix m1 m2)
; should give
;  30  36  42
;  66  81  96
; 102 126 150
(matrix-*-matrix m2 m2)

