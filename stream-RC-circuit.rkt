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


(define dt 0.05)
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

(define (integrate-delayed integrand v0 dt)
  (define int (stream-cons v0 (stream-add int (stream-scale (integrand) dt))))
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
  0)