#lang planet neil/sicp

(define (square x) (* x x))

; Louis' first attempt
(define (square-list1 items)
  (define (iter things answer)
    (if (null? things)
        answer
        (iter (cdr things)
              (cons (square (car things))
                    answer))))
  (iter items nil))

(square-list1 (list 1 2 3 4))

; square-list1 produces answers in reverse. 
; The problems lies with the statment
; (cons (square (car things)) answer)
;
; Consider the list 1 2 3 4
; This is the call tree
; (iter '(1 2 3 4) nil)
; (iter '(2 3 4) '(1))
; (iter '(3 4) '(4 1))
; (iter '(4) '(9 4 1))
; ...
; As can be seen the cons is computing the list inside out

; Louis' second attempt
(define (square-list2 items)
  (define (iter things answer)
    (if (null? things)
        answer
        (iter (cdr things)
              (cons answer
                    (square (car things))))))
  (iter items nil))

(square-list2 (list 1 2 3 4))

; square-list2 doesn't work either. It has a similar problem
; Consider the call tree with list 1 2 3 4
; (iter '(1 2 3 4) nil)
; (iter '(2 3 4) (cons nil 1))
; (iter '(3 4) (cons (cons nil 1) 4))
; ...
; Again the problem lies with the statement
; (cons answer (square (car things)))
; In the first iteration itself it produces (cons nil 1) 

; Here is a correct implementation using append
(define (square-list items)
  (define (iter things answer)
    (if (null? things)
        answer
        (iter (cdr things)
              (append answer (list (square (car things)))))))
  (iter items nil))

(square-list (list 1 2 3 4))

