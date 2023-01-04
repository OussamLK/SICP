#lang racket

(define (make-account amount)
  (define (deposit a)
    (set! amount (+ amount a))
    amount)
  (define (withdraw a)
    (set! amount (- amount a))
    amount)
  (lambda (dispatch)
    (cond ((eq? dispatch 'deposit) deposit)
          ((eq? dispatch 'withdraw) withdraw)
          (else (error "unknown command")))))

(define (protect f password)
  (lambda (v password_)
    (if (eq? password_ password) (f v) (error "password is not valid"))))

(define (protected-account amount password)
  (protect (make-account amount) password))

(define (make-joint acc old-pw new-pw)
  (protect (lambda (command)
             (acc command old-pw))
                new-pw))

(define peter-acc (protected-account 100 'open-sesame))
(define anna-acc (make-joint peter-acc 'open-sesame 'rosebud))

