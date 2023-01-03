#lang racket
(define (make-rand-gen a b)
  (lambda () (+ (* (random) (- b a)) a)))

(define (inc! v) (set! v (+ v 1)))

(define (mc-area pred x1 x2 y1 y2 trials)
  (let ((x (make-rand-gen x1 x2))
        (y (make-rand-gen y1 y2))
        (rec-area (* (- x2 x1) (- y2 y1)))
        (iteration 0)
        (valid-trials 0))
     (define (iter)
       (display iteration)
       (cond ((>= iteration trials) (* (/ valid-trials trials) rec-area))
             ((pred (x) (y)) (inc! valid-trials) (inc! iteration) (iter))
             (else (inc! iteration) (iter))))
     (iter)))

(define (module x y)
  (sqrt (+ (expt x 2) (expt y 2))))
(define (dist x1 y1 x2 y2)
  (module (- x1 x2) (- y1 y2)))
(define (in-unit-circle? x y)
  (<= (module x y) 1))

