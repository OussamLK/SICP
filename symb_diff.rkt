#lang racket/base


(define (add exp1 exp2) 
    (cond ((and (number? exp1) (number? exp2)) (+ exp1 exp2))
          ((and (number? exp1) (= exp1 0)) exp2)
          ((and (number? exp2) (= exp2 0)) exp1)
          (else (list '+ exp1 exp2)))
)

(define (multiply exp1 exp2)
    (cond ((and (number? exp1) (number? exp2)) (* exp1 exp2))
          ((and (number? exp1) (= exp1 0)) 0)
          ((and (number? exp2) (= exp2 0)) 0)
          ((and (number? exp1) (= exp1 1)) exp2)
          ((and (number? exp2) (= exp2 1)) exp1)
          (else (list '* exp1 exp2)))
)

(define (expo base exponent)
    (cond ((and (number? exponent) (= exponent 0)) 1)
          ((and (number? exponent) (= exponent 1)) base)
          (else (list '** base exponent)))

)

(define (is-sum? exp) (eq? (car exp) '+))
(define (is-multiply? exp) (eq? (car exp) '*))
(define (left exp) (cadr exp))
(define (right exp) (caddr exp))
(define (is-expo? exp) (and (list? exp) (eq? (car exp) '**)))


(define (diff exp var)
    (cond ((number? exp) 0)
          ((and (symbol? exp) (eq? exp var)) 1)
          ((symbol? exp) 0)
          ((is-sum? exp) (add (diff (left exp) var) (diff (right exp) var)))
          ((is-multiply? exp) (add (multiply (diff (left exp) var)
                                             (right exp))
                                   (multiply (left exp)
                                             (diff (right exp) var))
                              )
          )
          ((is-expo? exp) (multiply (diff (right exp) var) exp))
    )
)

(define exp (expo 2 (multiply 'x 'x)))



(define (display-line value)
    (display value)
    (display "\n"))
(display-line (diff exp 'x))
