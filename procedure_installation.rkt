#lang racket
(provide put
         get
         tag
         untag
         get-type
         get-content
         generic-operation
         set-type-hierarchy
         type-hierarchy
         get-subtype
         get-supertype
         )

(define (list-eq? l1 l2)
    (define (both-null a b) (and (null? a ) (null? b)))
    (define (not-null a) (not(null? a)))
    (define (none-null a b) (and (not-null a) (not-null b)))
    (or (both-null l1 l2)
        (and (none-null l1 l2)
             (and (eq? (car l1) (car l2))
                (list-eq? (cdr l1) (cdr l2))
                )
             )
        )
    )

(define proc-table '())
(define (cons-proc! proc-row)
    (set! proc-table (cons proc-row proc-table))
)

(define type-hierarchy '())
(define (set-type-hierarchy h) (set! type-hierarchy h))
(define (type-exists? t) (member t type-hierarchy))
(define (get-supertype t)
  (define (last lst) (if (null? lst) #f
                         (if (null? (cdr lst)) (car lst)
                             (last (cdr lst))))) 
  (if (not (eq? (last type-hierarchy) t)) (cadr (member t type-hierarchy)) #f)
  )
(define (get-subtype t) (if (not (eq? (car type-hierarchy) t)) (cadr (member t (reverse type-hierarchy))) #f ))

(define (coerce origin target)
  (if (not (and (type-exists? origin) (type-exists? target)))
      (error "Types do not exist")
      0
      )

  0)



(define (put procedure-name type-list procedure)
    (cons-proc! (list procedure-name type-list procedure)))
(define (get proc-name type-list)
    (define (get-procedure-name row) (car row))
    (define (get-type-list row) (cadr row))
    (define (get-procedure row) (caddr row))
    (define (search proc-name type-list table)
        (if (null? table) #f
            (let ((row (car table)))
                 (if (and (eq? (get-procedure-name row) proc-name)
                      (list-eq? (get-type-list row) type-list))
                        (get-procedure row)
                        (search proc-name type-list (cdr table))
                 )
            )
        )
    )
    (search proc-name type-list proc-table)
)

(define (tag tag var) (cons tag var))
(define (untag var) (cdr var))
(define (get-type var) (car var))
(define (get-content var) (cdr var))

(define (generic-operation op . args)
        (let ((proc (get op (map get-type args)))
              (vals (map get-content args)))
             (if proc (apply proc vals)
                      (error "no procedure available\n")))
) 


(define (demo)
    ;;you can use like so
    (put 'add '(int int) (lambda (x y) (+ x y)))
    (define n1 (tag 'int 3))
    (define n2 (tag 'int 4))
    (generic-operation 'add n1 n2)
)