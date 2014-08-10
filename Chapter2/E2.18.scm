#lang planet neil/sicp

(define (append-elem list item)
  (if (null? list) (cons item nil)
      (cons (car list) (append-elem (cdr list) item))))

(define (reverse list)
  (if (null? list) nil
      (append-elem (reverse (cdr list)) (car list))))