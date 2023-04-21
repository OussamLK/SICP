#lang racket
(require  racket/stream)
(define (op-streams op s1 s2)
  (if (or (stream-empty? s1) (stream-empty? s2)) empty-stream
      (stream-cons (op (stream-first s1) (stream-first s2)) (op-streams op (stream-rest s1) (stream-rest s2)))))

(define (interval-stream start end)
  (if (>= start end) empty-stream (stream-cons start (interval-stream (+ start 1) end))))
(define (first n s)
  (if (= n 0) 'done
      (begin (display (stream-first s))
             (display ", ")
             (first (- n 1) (stream-rest s)))))
(define (add-streams s1 s2) (op-streams + s1 s2))
(define (mult-streams s1 s2) (op-streams * s1 s2))
(define (partial-sums s)
 (define ps (stream-cons (stream-first s) (add-streams (stream-rest s) ps)))
  ps
 )

(define (merge s1 s2)
  (cond ((stream-empty? s1) s2)
        ((stream-empty? s2) s1)
        (else
         (let ((s1car (stream-first s1))
               (s2car (stream-first s2)))
           (cond ((< s1car s2car)
                  (stream-cons
                   s1car
                   (merge (stream-rest s1) s2)))
                 ((> s1car s2car)
                  (stream-cons
                   s2car
                   (merge s1 (stream-rest s2))))
                 (else
                  (stream-cons
                   s1car
                   (merge (stream-rest s1)
                          (stream-rest s2)))))))))
(define (scale-stream s n)
  (if (stream-empty? s) empty-stream (stream-cons (* n (stream-first s)) (scale-stream (stream-rest s) n))))
(define s (interval-stream 1 100))
(define ps (partial-sums s))
(define (divisible? n k) (= (remainder n k) 0))
(define d2 (stream-filter (lambda (n) (divisible? n 2)) s))
(define d3 (stream-filter (lambda (n) (divisible? n 3)) s))
(define H (stream-cons 1 (merge (scale-stream H 2) (merge (scale-stream H 3) (scale-stream H 5)))))
(define haming-stream (stream-filter (lambda (n) (or (divisible? n 2) (divisible? n 3) (divisible? n 5))) s))
(first 20 (stream-rest H))
(first 20 haming-stream)
