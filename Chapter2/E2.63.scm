#lang planet neil/sicp

(define (entry tree) (car tree))
(define (left-branch tree) (cadr tree))
(define (right-branch tree) (caddr tree))
(define (make-tree entry left right)
  (list entry left right))

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
                            (copy-to-list (right-branch tree) result-list)))))
  (copy-to-list tree '()))

(define (accumulate op initial sequence)
  (if (null? sequence)
      initial
      (op (car sequence)
          (accumulate op initial (cdr sequence)))))

; a. The two procedures produce the same results given the same trees
;    This is because they both generate the output in the same order
;    i.e (left branch), current node, (right branch)
;    The final output will list the numbers in ascending order
(define t1 (accumulate adjoin-set '() '(11 5 1 9 3 7)))
(define t2 (accumulate adjoin-set '() '(11 9 5 7 1 3)))
(define t3 (accumulate adjoin-set '() '(11 7 1 9 3 5)))

(tree->list-1 t1)
(tree->list-1 t2)
(tree->list-1 t3)
(tree->list-2 t1)
(tree->list-2 t2)
(tree->list-2 t3)

; b. Both procedures will have same order of growth in the number of steps
;    which will be O(n) where n is the number of elements in the tree
;    the reason being each procedure must look at each tree node atleast once
;    In terms of memory the growth will be O(lg(n)) where lg is log to base 2
;    because the depth of a balanced binary tree with n nodes is lg(n)
;    Although the tree->list-2 process looks like an interative process it
;    is also recursive because the result-list is computed in a recursive manner
