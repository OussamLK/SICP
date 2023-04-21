#lang racket
(require racket/stream)
(define (integrate-series s)
  (define (integrate-loop start-coef rest-s)
    (stream-cons (* (stream-first rest-s) (/ 1 (+ start-coef 1)))
                 (integrate-loop (+ 1 start-coef) (stream-rest rest-s))))
  (integrate-loop 0 s))
(define (alg op s1 s2)
  (stream-cons (op (stream-first s1) (stream-first s2)) (alg op (stream-rest s1) (stream-rest s2))))
(define (add s1 s2) (alg + s1 s2))
(define (scale f s) (stream-cons (* f (stream-first s)) (scale f (stream-rest s))))
  
(define s (stream-cons 1 s))
(define (show-first n s)
  (if (<= n 0) 'done
      (begin (display (stream-first s))
             (display ", ")
             (show-first (- n 1) (stream-rest s)))))
(define expo (stream-cons 1 (integrate-series expo)))
(define (list->stream lst ending)
  (if (null? lst) ending
      (stream-cons (car lst) (list->stream (cdr lst) ending))))
(define cosine-series (stream-cons 1 (scale -1 (integrate-series sine-series))))
(define sine-series (stream-cons 0 (integrate-series cosine-series)))
(define (mul-series s1 s2)
  (stream-cons (* (stream-first s1) (stream-first s2))
                (add (mul-series s1 (stream-rest s2)) (scale (stream-first s2) (stream-rest s1)))))
(define (square-s s) (mul-series s s))
(define (eval-1 s n) (if (= 0 n) 0 (+ (stream-first s) (eval-1 (stream-rest s) (- n 1))))) 

(define (invert-series s)
  ;;let 1/S = X => SX = 1 => (1 + Sr) X = 1 => X = 1 - XSr
  (let ((c (stream-first s)))
          (define s1 (scale (/ 1 c) s)) ;;Becase the trick only works with a leading term of 1
          (define inv-s1 (stream-cons 1 (scale -1 (mul-series inv-s1 (stream-rest s1)))))
          (scale (/ 1 c) inv-s1)))

(define (div-series n d)
  (mul-series n (invert-series d)))

(define (eval-series s x n)
  (define xs (stream-cons 1 (scale x xs)))
  (let ((res (alg * xs s)))
    (define (add-first n s)
      (if (<= n 0) 0
          (+ (stream-first s) (add-first (- n 1) (stream-rest s)))))
    (* 1.0 (add-first n res))))

(define tan-series (div-series sine-series cosine-series))

;;tests

;(show-first 10 cosine-series)
;(show-first 10 sine-series)
;(eval-1 (add (square-s cosine-series) (square-s sine-series)) 10)
(define pi 3.1415)
(define pi/2 (/ pi 2))
(define pi/4 (/ pi 4))
(define all-ones (stream-cons 1 all-ones))
(eval-series sine-series pi/4 20)
(eval-series cosine-series pi/4 20)
(eval-series tan-series (/ pi 3) 100)
(tan (/ pi 3))

