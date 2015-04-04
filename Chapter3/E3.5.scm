(define (square x) (* x x))

(define (random-in-range low high) 
  (let ((range (- high low))) 
    (+ low (random range))))

(define (monte-carlo trials experiment)
  (define (iter trials-remaining trials-passed)
    (cond ((= trials-remaining 0)
           (/ trials-passed trials))
          ((experiment)
           (iter (- trials-remaining 1)
                 (+ trials-passed 1)))
          (else
            (iter (- trials-remaining 1)
                  trials-passed))))
  (iter trials 0))

(define (circle-predicate x1 x2 y1 y2 x y)
  (let ((radius (/ (- x2 x1) 2))
        (cx (/ (+ x1 x2) 2))
        (cy (/ (+ y1 y2) 2)))
    (>= (square radius)
       (+ (square (- x cx)) (square (- y cy))))))

(define (estimate-integral p x1 x2 y1 y2 trials)
  (define (experiment)
    (let ((x (random-in-range x1 x2))
          (y (random-in-range y1 y2)))
      (p x1 x2 y1 y2 x y)))
  (let* ((side (- x2 x1))
         (area (square side))) 
    (* area (monte-carlo trials experiment))))

(estimate-integral circle-predicate -40 40 -40 40 100000) ; Should be close to 5024

;; `estimate-integral` gives us the area of circle with radius $r$
;; which is nothing but $\pi r ^ 2$. The circle is inscribed in a rectangle. 
;; The area of the rectangle is $(2r)^2$. 
;;
;; $$
;; \begin{aligned}
;; \frac{area\ of\ circle}{area\ of\ square} &= \frac{\pi r ^ 2}{(2r) ^ 2} \\
;; & = \frac{\pi r^2}{4 r^2} \\
;; & = \frac{\pi}{4} \\
;; \end{aligned}
;; $$
;;
;; Therefore
;; $$
;; \begin{aligned}
;; \frac{\pi}{4} &= \frac{area\ of\ circle}{area\ of\ square} \\
;; \pi & = 4 \frac{area\ of\ circle}{area\ of\ square}
;; \end{aligned}
;; $$

(define (estimate-pi)
  (* 4 (/ (estimate-integral circle-predicate -100 100 -100 100 100000)
          (square 200))))

(estimate-pi) ; should be close to 3.14
