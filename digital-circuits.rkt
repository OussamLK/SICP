#lang racket
(define (make-wire)
  (let ((signal 0)
        (run-on-update '()) ;a list of functions to be ran when the value changes
        )
    (define (run-updates)
      (define (loop update-functions)
        (if (null? update-functions) 'update-ran
            (begin ((car update-functions)) (loop (cdr update-functions)))))
      (loop run-on-update))
    (define (register-update-function! f)
      (set! run-on-update (cons f run-on-update)))
    (define (get-signal) signal)
    (define (set-signal! value)
      (set! signal value)
      (run-updates)
      )
    (define (dispatch m)
      (cond ((eq? m 'get-signal) (get-signal))
            ((eq? m 'set-signal!) set-signal!)
            ((eq? m 'register-update-function!) register-update-function!)
            (else (error "unknown function"))))
    dispatch
    ))

(define (set-action! wire fun)
  ;fun should have arity 0, this sounds like a bad design choice...
  ((wire 'register-update-function!) fun))

(define (after-delay delay-sec f)
  (display "sleeping for ") (display delay-sec) (display "seconds...")
  (f))

;;(define (make-and-gate in1 in2 out)
  

(define a (make-wire))
(define b (make-wire))
(define res (make-wire))


(define w1 (make-wire))
((w1 'register-update-function!) (lambda ()
                                   (display "signal has changed to: ")
                                   (display (w1 'get-signal))
                                   (display "\n")))