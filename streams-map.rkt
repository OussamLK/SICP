#lang racket
(require racket/promise)
(require racket/stream)

(define (stream-cons x y)
  (cons x (delay y)))
(define (stream-first s) (car s))
(define (stream-rest s) (force (cdr s)))
(define empty-stream '())
(define stream-empty? null?)

(define (list-to-stream l1)
  (if (null? l1) empty-stream
      (stream-cons (car l1) (list-to-stream (cdr l1)))))
(define (stream-to-list s1)
  (if (stream-empty? s1) '()
      (cons (stream-first s1) (stream-to-list (stream-rest s1)))))



(define (stream-map proc . streams)
  (if (stream-empty? (car streams))
      empty-stream
      (stream-cons (apply proc (map stream-first streams))
                   (apply stream-map (cons proc (map stream-rest streams))))))

;;tests

(define l1 '(1 2 3))
(define l2 '(3 5 8))
(define l3 '(10 20 30))
(define s1 (list-to-stream l1))
(define s2 (list-to-stream l2))
(define s3 (list-to-stream l3))

(define r (stream-map + s1 s2 s3))
(stream-to-list r)

;;printing
(define (stream-ref s1 n)
  (if (= 0 n) (stream-first s1)
      (stream-ref (stream-rest s1) (- n 1))))
(define (display-line x)
  (display x)
  (display "\n"))
(define (show x)
  (display-line x)
  x)

(define s10 (list-to-stream '(1 2 3 4 5 6 7 8 9 10)))
(define x (apply stream-map (cons show s10)))
(stream-ref x 5)

