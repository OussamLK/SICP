#lang racket/base
;; this creates smart object that dispatch the operation a bit like objects,
;;   rather than a smart operation that dispatches to the right specific operation wich is but like dunder methods in python
(define (make-complex x y)
    (define (dispatch op)
        (cond ((eq? op 'real) x)
              ((eq? op 'img) y)
              ((eq? op 'mag) (sqrt (+(* x x)(* y y))))))
    dispatch
)

(define z (make-complex 2 3))
(z 'mag)