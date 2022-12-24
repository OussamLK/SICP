#lang racket/base

(define (type-tag tag content) (cons tag content))
(define (get-type datum) (car datum))
(define (get-content datum) (cdr datum))

(define (make-complex-real-imag r i) (type-tag 'rect (cons r i)))
(define (make-complex-mag-ang m a) (type-tag 'polar (cons m a)))

(define (add-rect z1 z2)
    (let ((r1 (car (get-content z1)))
          (i1 (cdr (get-content z1)))
          (r2 (car (get-content z2)))
          (i2 (cdr (get-content z2))))
         
    (make-complex-real-imag (+ r1 r2) (+ i1 i2))
    )
)

(define (add-polar z1 z2)
    (let ((m1 (car (get-content z1)))
          (a1 (cdr (get-content z1)))
          (m2 (car (get-content z2)))
          (a2 (cdr (get-content z2))))
         
        (make-complex-real-imag (+ (* m1 (cos a1)) (* m2 (cos a2)))
                                (+ (* m1 (sin a1)) (* m2 (sin a2))))
    )
)

(define (add-complex z1 z2)
    (cond ((and (eq? (get-type z1) 'rect) (eq? (get-type z2) 'rect)) (add-rect z1 z2))
          ((and (eq? (get-type z1) 'polar) (eq? (get-type z2) 'polar) (add-polar z1 z2)))
          (else (error "type mismatch")))

)

(define z1 (make-complex-real-imag 2 3))
(define z2 (make-complex-real-imag 4 -1))
(define res (add-complex z1 z2))
(display res)

(define pi 3.1415)
(define z3 (make-complex-mag-ang 2 (/ pi 2)))
(define z4 (make-complex-mag-ang 4 (/ pi 1)))
(define res2 (add-complex z3 z4))
(display res2)

