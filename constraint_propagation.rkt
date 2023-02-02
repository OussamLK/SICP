#lang racket
(define (make-connector)
  (let ((value '())
        (setter '())
        (left-connector '())
        (right-connector '()))
    (define (connect connector)
      

      'done)
    (define (set-value! value_)
      (set! value value_)
      (set! setter 'user)
      'done)
    (define (dispatch m)
      (cond ((eq? m 'connect) connect)
            ((eq? m 'get-value) value)
            (else (error "unknown function"))))
    dispatch))

(define (make-adder connector-a connector-b connector-c  )
  ;; a+b=c constraint
  (let ((a connector-a)
        (b connector-b)
        (c connector-c))
    (define (compute)
      (define (get-free-connector)
        (define (get-first-null-connector lst)
          (if (null? lst) (error "can not find a free connector")
              (if (null? ((car lst) 'get-value)) (car lst) (get-first-connector (cdr lst)))))
        (define (count-free-connectors connectors)
          (if (null? connectors) 0
              (if (null? (car connectors) 'get-value) (+ 1 (cont-free-connectors (cdr connectors)))
                                                      (count-free-connectors (cdr connectors)))))
        (let ((nb-free-connectors (count-free-connectors (list a b c))))
             (cond ((= nb-free-connectors 1) (get-first-null-connector))
                   ((= nb-free-connectors e
          
      )
    (define (refresh)
      (compute))
    (define (dispatch m)
      (cond ((eq? m 'notify-change) refresh)
             (else (error "Adder: Unknown function"))))
    dispatch))
    