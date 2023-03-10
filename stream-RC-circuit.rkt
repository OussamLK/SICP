#lang racket


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
(first-n i (time-in-dt 11))
