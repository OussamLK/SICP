#lang racket/base
(define (square x) (* x x))
(define pi 3.1416)
(require "procedure_installation.rkt") ;;put, get, tag, untag, get-tag, get-content
(define (install-complex-rect)
    ;;internal to the package
    (define (make a b) (cons a b))
    (define (real z) (car z))
    (define (img z) (cdr z))
    (define (ang z) (atan (/ (img z) (real z))))
    (define (mag z) (sqrt (+ (square (real z)) (square (img z)))))

    ;;interface to the exterior
    (put 'make '(complex-rect) (lambda (a b) (tag 'complex-rect (make a b))))
    (put 'real '(complex-rect) real)
    (put 'img '(complex-rect) img)
    (put 'ang '(complex-rect) ang)
    (put 'mag '(complex-rect) mag)
)

(define (install-complex-polar)
    ;;internal to the package
    (define (make mag ang) (cons mag ang))
    (define (real z) (* (mag z) (cos (ang z))))
    (define (img z) (* (mag z) (sin (ang z))))
    (define (ang z) (cdr z))
    (define (mag z) (car z))


    ;;interface to the exterior
    (put 'make '(complex-polar) (lambda (a b) (tag 'complex-polar (make a b))))
    (put 'real '(complex-polar) real)
    (put 'img '(complex-polar) img)
    (put 'ang '(complex-polar) ang)
    (put 'mag '(complex-polar) mag)
)

(define (install-rational-numbers)
    ;;internal
    (define (make num denom)
        (let ((d (gcd num denom)))
                (cons (/ num d) (/ denom d))
            )
    )
    (define (num q) (car q))
    (define (denom q) (cdr q))
    (define (add q1 q2)
        (let ((common (* (denom q1) (denom q2)))
              (nq1 (num q1))
              (nq2 (num q2))
              (dq1 (denom q1))
              (dq2 (denom q2))
            )
            (make (+ (* nq1 dq2) (* nq2 dq1)) common))
        )
    ;;external
    (put 'make '(rational) (lambda (a b) (tag 'rational (make a b))))
    (put 'add '(rational rational) (lambda (q1 q2) (tag 'rational (add q1 q2))))

)

(define (install-complex-numbers)
    ;;prerequ
    (install-complex-rect)
    (install-complex-polar)
    (define (real z) (generic-operation 'real z))
    (define (img z) (generic-operation 'img z))
    (define (mag z) (generic-operation 'mag z))
    (define (ang z) (generic-operation 'ang z))
    (define make-complex-rect (get 'make '(complex-rect)))
    (define make-complex-polar (get 'make '(complex-polar)))

    ;;interface
    (define (add z1 z2)
        (let ((r (+(real z1) (real z2)))
              (i (+ (img z1) (img z2))))
              (make-complex-rect r i)))
    (define (subtract z1 z2)
        (let ((r (-(real z1) (real z2)))
              (i (- (img z1) (img z2))))
              (make-complex-rect r i)))
    (define (mult z1 z2)
            (let ((mag_ (* (mag z1) (mag z2)))
                  (ang_ (+ (ang z1) (ang z2))))
                            (make-complex-polar mag_ ang_)
                            ))
    (define (div z1 z2)
            (let ((mag_ (/ (mag z1) (mag z2)))
                  (ang_ (- (ang z1) (ang z2))))
                            (make-complex-polar mag_ ang_)))
   ;;external
   (put 'make-from-rect '(complex) (lambda (r i) (tag 'complex (make-complex-rect r i))))
   (put 'make-from-polar '(complex) (lambda (r i) (tag 'complex (make-complex-polar r i))))
   (put 'add '(complex complex) (lambda (z1 z2) (tag 'complex (add z1 z2)))) 
   (put 'subtract '(complex complex) (lambda (z1 z2) (tag 'complex (subtract z1 z2)))) 
   (put 'mult '(complex complex) (lambda (z1 z2) (tag 'complex (mult z1 z2)))) 
   (put 'div '(complex complex) (lambda (z1 z2) (tag 'complex (div z1 z2)))) 

)




(install-rational-numbers)
(install-complex-numbers)
(define make-complex-rect (get 'make-from-rect '(complex)))
(define (add z1 z2) (generic-operation 'add z1 z2))
(define (mult z1 z2) (generic-operation 'mult z1 z2))
(define (div z1 z2) (generic-operation 'div z1 z2))

(let ((z1 (make-complex-rect 0.0000000001 1))
      (z2 (make-complex-rect 2 0))
     )
    (add z1 z2)
    (mult z1 z2)
    (div z1 z2)

)

