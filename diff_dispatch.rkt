#lang racket/base
(define (eq_lists l1 l2)
    (let ((nl1 (null? l1))
          (nl2 (null? l2)))
         (if (or nl1 nl2)
                (and nl1 nl2)
                
                (and (eq? (car l1) (car l2))
                     (eq_lists (cdr l1) (cdr l2))))))

(define (derv-int n) n )

(put 'derv '(int) derv-int)