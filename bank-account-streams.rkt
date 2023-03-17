#lang racket
(define (bank-account withdrawals initial)
  (stream-cons initial (bank-account (stream-rest withdrawals) (- initial (stream-first withdrawals)))))

(define (first-n stream n)
  (if (<= n 0) 'done
      (begin (display (stream-first stream))
             (display ", ")
             (first-n (stream-rest stream) (- n 1)))))

(define withdrawals (stream-cons 2 withdrawals))
(define balance-stream (bank-account withdrawals 200))
(first-n balance-stream 10)