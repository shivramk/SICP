#lang planet neil/sicp

(define (entry tree) (car tree))
(define (left-branch tree) (cadr tree))
(define (right-branch tree) (caddr tree))
(define (make-tree entry left right)
  (list entry left right))

(define (element-of-set? x set)
  (cond ((null? set) false)
        ((= x (entry set)) true)
        ((< x (entry set))
         (element-of-set? x (left-branch set)))
        ((> x (entry set))
         (element-of-set? x (right-branch set)))))

(define (adjoin-set x set)
  (cond ((null? set) (make-tree x '() '()))
        ((= x (entry set)) set)
        ((< x (entry set))
         (make-tree (entry set)
                    (adjoin-set x (left-branch set))
                    (right-branch set)))
        ((> x (entry set))
         (make-tree (entry set)
                    (left-branch set)
                    (adjoin-set x (right-branch set))))))

(define (tree->list-1 tree)
  (if (null? tree)
      '()
      (append (tree->list-1 (left-branch tree))
              (cons (entry tree)
                    (tree->list-1 (right-branch tree))))))

(define (tree->list-2 tree)
  (define (copy-to-list tree result-list)
    (if (null? tree)
        result-list
        (copy-to-list (left-branch tree)
                      (cons (entry tree)
                            (copy-to-list (right-branch tree)
                                          result-list)))))
  (copy-to-list tree '()))

(define (list->tree elements)
  (car (partial-tree elements (length elements))))

(define (partial-tree elts n)
  (if (= n 0)
      (cons '() elts)
      (let ((left-size (quotient (- n 1) 2)))
        (let ((left-result (partial-tree elts left-size)))
          (let ((left-tree (car left-result))
                (non-left-elts (cdr left-result))
                (right-size (- n (+ left-size 1))))
            (let ((this-entry (car non-left-elts))
                  (right-result (partial-tree (cdr non-left-elts)
                                              right-size)))
              (let ((right-tree (car right-result))
                    (remaining-elts (cdr right-result)))
                (cons (make-tree this-entry left-tree right-tree)
                      remaining-elts))))))))

; O(n) impelementation of union on ordered lists
; used by union-set
(define (union-list set1 set2)
  (cond ((null? set1) set2)
        ((null? set2) set1)
        (else
         (let ((x1 (car set1)) (x2 (car set2)))
           (cond ((= x1 x2)
                  (union-list (cdr set1) set2))
                 ((> x1 x2)
                  (cons x2
                        (union-list set1 (cdr set2))))
                 (else
                  (cons x1
                        (union-list (cdr set1) set2))))))))

; O(n) impelementation of intersection on ordered lists
; used by intersection-set
(define (intersection-list set1 set2)
  (if (or (null? set1) (null? set2))
      '()
      (let ((x1 (car set1)) (x2 (car set2)))
        (cond ((= x1 x2)
               (cons x1
                     (intersection-list (cdr set1)
                                        (cdr set2))))
              ((< x1 x2)
               (intersection-list (cdr set1) (cdr set2)))
              ((< x2 x1)
               (intersection-list set1 (cdr set2)))))))

; O(n) implementations of union-set and intersection-set
(define (union-set set1 set2)
  (let ((l1 (tree->list-2 set1))
        (l2 (tree->list-2 set2)))
    (let ((merged-list (union-list l1 l2)))
      (list->tree merged-list))))

(define (intersection-set set1 set2)
  (let ((l1 (tree->list-2 set1))
        (l2 (tree->list-2 set2)))
    (let ((merged-list (intersection-list l1 l2)))
      (list->tree merged-list))))

(define (test-proc proc l1 l2)
  (let ((tree1 (list->tree l1))
        (tree2 (list->tree l2)))
    (tree->list-2 (proc tree1 tree2))))

(test-proc union-set '() '())
(test-proc union-set '(1 2 3) '())
(test-proc union-set '(1 2 3) '(1 2 4))
(test-proc union-set '() '(1 2 3))
(test-proc union-set '(1 2) '(3 4))

(test-proc intersection-set '() '())
(test-proc intersection-set '(1) '(1))
(test-proc intersection-set '(1 2 3) '(1 2 4))
(test-proc intersection-set '() '(1 2 3))
(test-proc intersection-set '(1 2) '(1 3 4))
