#lang racket
(require rnrs/mutable-pairs-6)
(define cons mcons)
(define set-car! set-mcar!)
(define set-cdr! set-mcdr!)
(define car mcar)
(define cdr mcdr)
(define nil '())


(define (named-1-table name same-key?)
  (let ((head (cons name nil)))
    (define (insert-value! key value)
      (let ((new-pair (cons key value)))
        (let ((new-ptr (cons new-pair (cdr head))))
          (set-cdr! head new-ptr))))
    (define (get-value key)
      (define (lookup key tail)
        (cond ((null? tail) #f)
              ((same-key? (caar tail)) (cdar tail))
              (else (lookup key (cdr tail)))))
      (lookup key (cdr head)))
            

    (define (dispatch m)
      (cond ((eq? m 'insert-value!) insert-value!)
            ((eq? m 'get-value) get-value)
            (else (error "unknown function"))))
    dispatch))

(define t1 (named-1-table 'my-table eq?))
((t1 'insert-value!) 'name 'oussam)