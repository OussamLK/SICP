#lang racket
(define (make-rand-gen a b)
  (lambda () (+ (* (random) (- b a)) a)))

(define (inc! v) (set! v (+ v 1))) ;;this does not work, strange

(define (mc-area pred x1 x2 y1 y2 trials)
  (let ((x (make-rand-gen x1 x2))
        (y (make-rand-gen y1 y2))
        (rec-area (* (- x2 x1) (- y2 y1)))
        (iteration 0)
        (valid-trials 0))
     (define (iter)
       (cond ((>= iteration trials) (* 1.0 (/ valid-trials trials) rec-area))
             ((pred (x) (y)) (set! valid-trials (+ 1 valid-trials)) (set! iteration (+ 1 iteration)) (iter))
             (else (set! iteration (+ 1 iteration)) (iter))))
     (iter)))

(define (module x y)
  (sqrt (+ (expt x 2) (expt y 2))))
(define (dist x1 y1 x2 y2)
  (module (- x1 x2) (- y1 y2)))
(define (in-unit-circle? x y)
  (<= (module x y) 1))

(define (test trials)
  (mc-area in-unit-circle? -1 1 -1 1 trials))