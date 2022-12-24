#lang racket/base

(define type-hierarchy (list 'n 'z 'r 'c))
(define (coerce t1 t2 t-h)
    (define (find t)
        (define (find-loop t t-h i)
            (cond ((null? t-h) #f)
                  ((eq? t (car t-h)) i)
                  (else (find-loop t (cdr t-h) (+ 1 i))))
        )
        (find-loop t t-h 0)
    )
    (define (chop t t-h)
        (cond ((null? t-h) #f)
              ((eq? t (car t-h)) t-h)
              (else (chop t (cdr t-h)))))
    (define (move-n-up t n)
        (define (move-one-up t)
            (list t '-> (cadr (chop t t-h)) '..))
        (if (= 0 n) '() (append (move-one-up t) (move-n-up (cadr (chop t t-h)) (- n 1)))))
        
    
    (let ((p1 (find t1))
          (p2 (find t2)))
            (cond ((= p1 p2) 'nothing)
                  ((> p1 p2) (move-n-up t2 (- p1 p2)))
                  (else (move-n-up t1 (- p2 p1)))
                  )
    )

)

(coerce 'n 'c type-hierarchy)
