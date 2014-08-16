#lang scheme

(define (make-leaf symbol weight)
  (list 'leaf symbol weight))

(define (leaf? object)
  (eq? (car object) 'leaf))

(define (symbol-leaf x) (cadr x))
(define (weight-leaf x) (caddr x))

(define (make-code-tree left right)
  (list left
        right
        (append (symbols left) (symbols right))
        (+ (weight left) (weight right))))

(define (left-branch tree) (car tree))
(define (right-branch tree) (cadr tree))

(define (symbols tree)
  (if (leaf? tree)
      (list (symbol-leaf tree))
      (caddr tree)))

(define (weight tree)
  (if (leaf? tree)
      (weight-leaf tree)
      (cadddr tree)))

(define (decode bits tree)
  (define (decode-1 bits current-branch)
    (if (null? bits)
        '()
        (let ((next-branch
               (choose-branch (car bits) current-branch)))
          (if (leaf? next-branch)
              (cons (symbol-leaf next-branch)
                    (decode-1 (cdr bits) tree))
              (decode-1 (cdr bits) next-branch)))))
  (decode-1 bits tree))

(define (choose-branch bit branch)
  (cond ((= bit 0) (left-branch branch))
        ((= bit 1) (right-branch branch))
        (else (error "bad bit -- CHOOSE BRANCH" bit ))))

(define (adjoin-set x set)
  (cond ((null? set) (list x))
        ((< (weight x) (weight (car set))) (cons x set))
        (else (cons (car set)
                    (adjoin-set x (cdr set))))))

(define (make-leaf-set pairs)
  (if (null? pairs)
      '()
      (let ((pair (car pairs)))
        (adjoin-set (make-leaf (car pair)
                               (cadr pair))
                    (make-leaf-set (cdr pairs))))))

(define (encode message tree)
  (if (null? message)
      '()
      (append (encode-symbol (car message) tree)
              (encode (cdr message) tree))))

(define (has-symbol? sym node)
  (define (search l symbol)
    (cond ((null? l) #f)
          ((eq? sym (car l)) #t)
          (else (search (cdr l) symbol))))
  (search (symbols node) sym))

(define (encode-symbol symbol tree)
  (define (encode-1 branch bits)
    (cond ((leaf? branch) bits)
          ((has-symbol? symbol (left-branch branch))
           (encode-1 (left-branch branch) (append bits '(0))))
          ((has-symbol? symbol (right-branch branch))
           (encode-1 (right-branch branch) (append bits '(1))))
          (else (error "Symbol not found" symbol))))
  (encode-1 tree '()))

(define (generate-huffman-tree pairs)
  (successive-merge (make-leaf-set pairs)))

(define (successive-merge set)
  (if (null? (cdr set))
      (car set)
      (successive-merge 
       (adjoin-set (make-code-tree (car set) (cadr set))
                   (cddr set)))))

(define lyrics-tree (generate-huffman-tree 
                     '((na 16) (yip 7) (sha 3) (get 2) 
                               (job 2) (a 2) (boom 1) (wah 1))))

(define lyrics '(get a job
                     sha na na na na na na na na
                     get a job
                     sha na na na na na na na na
                     wah yip yip yip yip yip yip yip yip yip
                     sha boom))

(define encoded-lyrics (encode lyrics lyrics-tree))
(length encoded-lyrics) ; 86 bits

; With a fixed length code, minimum number of bits will be 3 for each symbol
; message length in bits = 3 * number of symbols
(* 3 (length lyrics)) ; 108 bits
