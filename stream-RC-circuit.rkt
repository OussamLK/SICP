#lang racket

(define N (stream-cons 0 (stream-shift N 1)))

(define (first-n s n)
  (if (= n 0) 'done
      (begin (display (stream-first s))
             (display ", ")
             (first-n (stream-rest s) (- n 1)))))


(define (stream-shift s n)
  (stream-map (lambda (si) (+ si n)) s))


(define (stream-scale s a)
  (stream-map (lambda (e) (* e a)) s))

(define (stream-add s1 s2)
  (stream-cons ( + (stream-first s1) (stream-first s2)) (stream-add (stream-rest s1) (stream-rest s2))))

(define (integrate integrand v0 dt)
  (define int (stream-cons v0 (stream-add int (stream-scale integrand dt))))
  int)

; The solution to a modified version of exercise 3.73, I think this makes more sense
;     I solve the differential equation numerically


; v = 1/c int(i dt) + Ri
; i = [v - 1/C int(i dt)]/ R
; i0 = v/R

;use stream-add s1 s2
;    stream-shift s x
;    stream-scale s x


(define dt 0.001)
(define (time-in-dt t) (* 1.0  (/ t  dt)))



(define (get-i v C R)
  (let ((i0 (* 1.0 (/ v R)))
       (neg1/c (/ -1.0 C))
       (1/R  (/ 1.0 R)))
  (define i (stream-cons i0 (stream-scale (stream-shift (stream-scale (integrate i 0 dt)
                                                                      neg1/c)
                                                        v)
                                          1/R)))
    i))


(define i (get-i 5 1 5))
;(first-n i (time-in-dt 11))


;; exercise 3.74
(define (stream-map_ f . streams)
  (stream-cons (apply f (map stream-first streams))
               (apply stream-map_ (cons f (map stream-rest streams)))))

(define (crossing input-stream)
  (define (crossing-function x0 x1)
    (let ((prod (* x0 x1)))
    (if (>= prod 0) 0
                   (if (< x0 0) 1 -1))))
  (stream-map_ crossing-function input-stream (stream-rest input-stream)))

;; ex 3.75 done on paper
;; ex 3.76

(define (smooth s)
  (define (avg a b) (/ (+ a b) 2))
  (stream-map_ avg s (stream-rest s)))

(define (smoothed-zero-crossing smoothing-function s)
  (crossing (smoothing-function s)))

;; streams and delayed evaluation: 3.5.4

(define (integrate-delayed integrand-promise v0 dt)
  (define int (stream-cons v0 (stream-add int (stream-scale (force integrand-promise) dt))))
  int)

;;solving equation dy/dt = f(y), and y(0) = y0

(define (solve-eq1 f y0 dt)
  (define (fy) (stream-map f y))
  (define y (integrate-delayed fy y0 dt))
  y)

;; ex 3.77

(define (integrate-delayed2 integrand v0 dt)
  (stream-cons v0
               (integrate-delayed2 (lambda () (stream-rest (integrand)))
                                   (+ v0 (* dt (stream-first (integrand))))
                                   dt)))

(define (solve-eq1-v2 f y0 dt)
  (define (fy) (stream-map f y))
  (define y (integrate-delayed2 fy y0 dt))
  y)

;;tests
;(define e (solve-eq1-v2 (lambda (x) x) 1 0.001))
;(stream-ref e 1000)

;; ex 3.78
(define (solve-2nd a b y0 dy0)
  (define y (integrate-delayed (delay dy) y0 dt))
  (define dy (integrate-delayed (delay d2y) dy0 dt))
  (define d2y (stream-add (stream-scale dy a) (stream-scale y b)))
  y)

;; ex 3.79

(define (solve-2nd-gen f a b y0 dy0)
  (define y (integrate-delayed (delay dy) y0 dt))
  (define dy (integrate-delayed (delay d2y) dy0 dt))
  (define d2y (stream-map_ f dy y))
  y)

;; ex 3.80

(define (RLC R L C vc0 il0)
  (map display (list "R=" R "; L=" L "; C=" C "; vc0=" vc0 "; il0=" il0 "\n")) 
  (define vc (stream-scale (integrate-delayed (delay i) il0 dt) (/ -1.0 C)))
  (define i (stream-add (stream-scale (integrate-delayed (delay vc)  vc0 dt) (/ 1.0 L))
                        (stream-scale (integrate-delayed (delay i) il0 dt) (/ (* -1.0 R) L))))
  (stream-map_ cons vc i))

;;tests
;(set! dt 0.1)
;(define vi-stream (RLC 1 1 0.2 10 0))
;(stream-ref vi-stream 100)

; Exercise 3.81

(define (next-rand val)
  ;; just for the sake of testing
  (+ val 1))

(define (generator next-rand stream prev)
  (let ((f (stream-first stream)))
    (if (number? f) (stream-cons f (generator next-rand (stream-rest stream) f))
        (let ((new (next-rand prev)))
          (stream-cons new (generator next-rand (stream-rest stream) new))))))
  
;; testing
(define (list->stream lst)
  (if (null? lst) '() (stream-cons (car lst) (list->stream (cdr lst)))))

(define test-stream (list->stream '(5 gen gen 3 gen gen gen 4 gen gen)))
(define g (generator next-rand test-stream 0))
;(first-n g 10)

; exercise 3.82

(define (estimate sample)
  (define (rest successes total)
       (if (sample) (stream-cons (/ (+ successes 1) (+ total 1)) (rest (+ successes 1) (+ total 1)))
                    (stream-cons (/ successes (+ total 1)) (rest  successes (+ total 1)))))
  (rest 0.0 0)
  )

(define (monte-carlo f max a b)
  (define (sample a b)
    (+ (* (random 1000) (/ (- b a) 1000.0)) a))
  (define (under)
    (let ((x (sample a b))
          (y (sample 0 max)))
      (< y (f x))))
  (stream-scale (estimate under) (* max (- b a))))

;tests

(define (square x) (* x x))
(define x3/3 (monte-carlo square 4 -2 2))
(stream-ref x3/3 1000)