#lang planet neil/sicp

(define (accumulate op initial sequence)
  (if (null? sequence)
      initial
      (op (car sequence)
          (accumulate op initial (cdr sequence)))))

(define (map p sequence)
  (accumulate (lambda (x y) (cons (p x) y)) nil sequence))

(map (lambda (x) (* x x)) (list 1 2 3 4)) ; (1 4 9 16)

(define (append seq1 seq2) 
  (accumulate cons seq2 seq1))

(append (list 1 2 3) (list 4 5 6)) ; (1 2 3 4 5 6)
  
(define (length sequence) 
  (accumulate (lambda (x y) (inc x)) 0 sequence))

(length nil) ; 0
(length (list 1 2 3)) ; 2