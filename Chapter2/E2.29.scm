#lang scheme

(define (make-mobile left right)
  (list left right))

(define (make-branch length structure)
  (list length structure))

; a. selectors
(define (left-branch node)
  (car node))

(define (right-branch node)
  (cadr node))

(define (branch-length node)
  (car node))

(define (branch-structure node)
  (cadr node))

; b. total weight
(define (total-weight node)
  (if (pair? (branch-length node))
      (+ (total-weight (left-branch node))
         (total-weight (right-branch node)))
      (if (not (pair? (branch-structure node)))
          (branch-structure node)
          (total-weight (branch-structure node)))))

(define branch1 (make-branch 5 10))
(define branch2 (make-branch 3 11))
(define branch3 (make-branch 3 12))
(define branch4 (make-branch 2 (make-mobile branch2 branch3)))
(define mobile (make-mobile branch1 branch4))

(total-weight mobile); 33

; c. balanced
(define (balanced node)
  (define (torque node)
    (if (pair? (branch-length node))
        (let ((left-torque (torque (left-branch node)))
              (right-torque (torque (right-branch node))))
          (if (= left-torque right-torque)
              (+ (total-weight (left-branch node))
                 (total-weight (right-branch node)))
              -1))
        (if (not (pair? (branch-structure node)))
            (* (branch-structure node) (branch-length node))
            (* (branch-length node) (torque (branch-structure node))))))
  (> (torque node) 0))

(define mobile2 (make-mobile branch1 branch1))

(balanced mobile) ; #f
(balanced mobile2) ; #t

; d. If we change the representation to this
; (define (make-mobile left right) (cons left right))
; (define (make-branch length structure)
;    (cons length structure))
; 
; No changes are required to the programs because the programs only make use
; of the selectors and don't try to access the data directly