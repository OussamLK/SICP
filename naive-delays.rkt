#lang racket
(define calls 0)
(define (force f)
  (f))
(define s (cons 1 (lambda () (add s s))))
(define (count-calls f . args)
  (set! calls (+ calls 1))
  (display "calling scale ")
  (display calls)
  (display "\n")
  (apply f args))
  
(define (add s1 s2)
  (cons (+ (car s1) (car s2)) (lambda () (add-counted (cdrs s1) (cdrs s2)))))
(define (add-counted s1 s2) (count-calls add s1 s2))
(define (cdrs s)
  (force (cdr s)))


