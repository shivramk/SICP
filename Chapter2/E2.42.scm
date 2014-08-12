#lang planet neil/sicp

(define (filter predicate sequence)
  (cond ((null? sequence) '())
        ((predicate (car sequence))
         (cons (car sequence)
               (filter predicate (cdr sequence))))
        (else
         (filter predicate (cdr sequence)))))

(define (accumulate op initial sequence)
  (if (null? sequence)
      initial
      (op (car sequence)
          (accumulate op initial (cdr sequence)))))

(define (flatmap proc seq)
  (accumulate append '() (map proc seq)))

(define (enumerate-interval low high)
  (if (> low high)
      '()
      (cons low (enumerate-interval (+ low 1) high))))

(define empty-board nil)

(define (make-position r c) (cons r c))
(define (row p) (car p))
(define (col p) (cdr p))

(define (adjoin-position new-row k rest-of-queens)
  (cons (make-position new-row k) rest-of-queens))

(define (check-row p queens)
  (null? (filter (lambda (x) (= (row p) (row x))) queens)))

(define (check-diag p queens)
  (null? 
   (filter (lambda (y) (= (abs (row y)) (abs (col y))))
           (map (lambda (x) 
                  (make-position (- (row p) (row x))
                                 (- (col p) (col x)))) queens))))

(define (safe? k positions)
  (let ((current-position (car positions))
        (rest-of-queens (cdr positions)))
    (or (= k 1)
        (and (check-row current-position rest-of-queens)
             (check-diag current-position rest-of-queens)))))

(define (queens board-size)
  (define (queen-cols k)
    (if (= k 0)
        (list empty-board)
        (filter
         (lambda (positions) (safe? k positions))
         (flatmap
          (lambda (rest-of-queens)
            (map (lambda (new-row)
                   (adjoin-position new-row
                                    k
                                    rest-of-queens))
                 (enumerate-interval 1 board-size)))
          (queen-cols (- k 1))))))
  (queen-cols board-size))

(queens 8)