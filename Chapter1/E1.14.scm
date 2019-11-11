;; This is the tree for (cc 11 3)
;; 
;; ![Tree for `(cc 11 3)`](Chapter1/images/E1_14_1)
;; 
;; #### The following program was used to generate the above tree

(import (chicken format))

(define (cc amount kinds-of-coins) 
  (printf "\\documentclass[tikz,border=10pt]{standalone}
          \\usetikzlibrary{graphdrawing}
          \\usetikzlibrary{graphs}
          \\usegdlibrary{trees}
          \\begin{document}
          \\begin{tikzpicture}[>=stealth, every node/.style={minimum size=0.75cm}]
          \\graph [tree layout, grow=down, fresh nodes, level distance=0.5in, 
                       sibling distance=0.5in]
              {~n")
  (cc_ amount kinds-of-coins 0)
  (printf "};
          \\end{tikzpicture}
          \\end{document}~n"))

(define (gap c)
  (cond ((= c 0) "")
        (else (string-append " " (gap (- c 1))))))

(define (cc_ amount kinds-of-coins depth)
  (begin (printf "~a\"\\texttt{(cc ~s ~s)}\"" (gap depth) amount kinds-of-coins)
         (cond ((= amount 0) 
                (begin (printf ",~n") 1))
               ((or (< amount 0) (= kinds-of-coins 0)) 
                (begin (printf ",~n") 0))
               (else 
                 (begin (printf " -> {~n")
                        (let ((ret (+ (cc_ amount
                                           (- kinds-of-coins 1) (+ depth 1))
                                      (cc_ (- amount
                                              (first-denomination
                                                kinds-of-coins))
                                           kinds-of-coins (+ depth 1)))))
                          (begin (printf "~a},~n" (gap depth)) ret)))))))

(define (first-denomination kinds-of-coins)
  (cond ((= kinds-of-coins 1) 1)
        ((= kinds-of-coins 2) 5)
        ((= kinds-of-coins 3) 10)
        ((= kinds-of-coins 4) 25)
        ((= kinds-of-coins 5) 50)))

(cc 11 3)

;; #### Complexity Analysis (Space)
;; 
;; The maximum recursion depth will be reached with coins of denomination 1.
;; Assuming the amount is n, space growth will be $O(n)$ as there will be recursive
;; calls with amount $n, n-1, n-2\ ...$ all the way up to $0$.
;; 
;; #### Complexity Analysis (Time)
;; 
;; 1. In the base case with coins of a single denomination it is easy to see that
;;    the steps will be proportional to the amount i.e O(n).
;; 
;; 2. With coins of two denominations, the number of steps will be roughly
;;    equal to
;; 
;;     $O(n) + O(n-k) + O(n-2k) + ...\ (n/k\ times)$
;; 
;;     Which is nothing but $O(n^2)$
;; 
;; 3. We can extrapolate from here and reach the conclusion that the number of
;;    steps will be $O(n^k)$
