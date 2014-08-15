#lang planet neil/sicp

(define (adjoin-set x set)
  (cons x set))

(adjoin-set 3 '(1 2 3))

(define (element-of-set? x set)
  (cond ((null? set) false)
        ((equal? x (car set)) true)
        (else (element-of-set? x (cdr set)))))

(element-of-set? 3 '(1 2 2 1))
(element-of-set? 3 '())
(element-of-set? 3 '(1 2 1 2 2 3))

; repitition is fine so just append the two lists
(define (union-set set1 set2)
  (append set1 set2))

(union-set '() '(1 2 2))
(union-set '(1 2 3) '(1 4 5 2 1 2 3))

; this can stay the same
(define (intersection-set set1 set2)
  (cond ((or (null? set1) (null? set2)) '())
        ((element-of-set? (car set1) set2)
         (cons (car set1)
               (intersection-set (cdr set1) set2)))
        (else (intersection-set (cdr set1) set2))))

(intersection-set '(1 2 3 3) '(1 4 5 2 1 2 3))

; How does the efficieny compare
; 1. adjoin-set - O(1)
; 2. element-of-set - O(n) where n is the number of elements in the list
; 3. union-set - O(1)
; 4. intersection-set O(n^2)
;
; Although element-of-set and intersection-set have same order of growth
; as earlier, they will be slower when we have more duplicate elements in 
; the list
; 
; If we know that a particular data set has very few duplicates
; then this representation will actually be faster because adjoin-set 
; and union-set are O(1). Another use case might be that we want a count
; of each occurence of a value which can be extracted from this representation

