#lang planet neil/sicp

; A record consists of a key and value
(define (make-record key value) (cons key value))
(define (key record) (car record))
(define (value record) (cdr record))

; entry assumes that the record will have a method called key
; other methods remain unchanged
(define (entry tree) (key (record tree)))
(define (record tree) (car tree))
(define (left-branch tree) (cadr tree))
(define (right-branch tree) (caddr tree))
(define (make-tree record left right)
  (list record left right))

(define (lookup given-key set-of-records)
  (cond ((null? set-of-records) false)
        ((= given-key (entry set-of-records)) (record set-of-records))
        ((< given-key (entry set-of-records))
         (lookup given-key (left-branch set-of-records)))
        ((> given-key (entry set-of-records))
         (lookup given-key (right-branch set-of-records)))))

(define (adjoin-records r set-of-records)
  (cond ((null? set-of-records) (make-tree r '() '()))
        ((= (key r) (entry set-of-records)) set-of-records)
        ((< (key r) (entry set-of-records))
         (make-tree (record set-of-records)
                    (adjoin-records r (left-branch set-of-records))
                    (right-branch set-of-records)))
        ((> (key r) (entry set-of-records))
         (make-tree (record set-of-records)
                    (left-branch set-of-records)
                    (adjoin-records r (right-branch set-of-records))))))

; used for testing
(define (accumulate op initial sequence)
  (if (null? sequence)
      initial
      (op (car sequence)
          (accumulate op initial (cdr sequence)))))

(define r1 (make-record 1 'lisp))
(define r2 (make-record 3 'scheme))
(define r3 (make-record 6 'python))
(define r4 (make-record 9 'haskell))
(define r5 (make-record 10 'prolog))

(define t1 (accumulate adjoin-records '() (list r5 r2 r1 r4 r3)))

(lookup 5 t1)
(lookup 1 t1)
(lookup 10 t1)
(lookup 0 t1)