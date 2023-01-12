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



(define (make-and-gate in1 in2 out)
  (define and-delay 1)
  (define (band v1 v2)
    (if (and (= v1 1) (= v2 1)) 1 0))
  (define (delayed-update)
    (let ((new-output (band (in1 'get-signal) (in2 'get-signal))))
      (after-delay and-delay (lambda () ((out 'set-signal!) new-output)))))
  (delayed-update) ;;set the output on creation
  (set-action! in1 delayed-update)
  (set-action! in2 delayed-update))

(define (make-not-gate in out)
  (define not-delay 0.5)
  (define (bnot in)
    (if (= 0 in) 1 0))
  (define (delayed-update)
    (let ((new-output (bnot (in 'get-signal))))
      (after-delay not-delay (lambda () ((out 'set-signal!) new-output)))))
  (delayed-update)
  (set-action! in delayed-update))

(define (make-or-gate in1 in2 out)
  (let ((w1 (make-wire))
        (w2 (make-wire))
        (w3 (make-wire)))
    (let ((not1 (make-not-gate in1 w1))
          (not2 (make-not-gate in2 w2))
          (inner-and (make-and-gate w1 w2 w3))
          (last-not (make-not-gate w3 out)))
      'done
      )))




(define a (make-wire))
(define b (make-wire))
(define res (make-wire))
(define res2 (make-wire))
(define and-gate1 (make-and-gate a b res))
(define nand1 (make-not-gate res res2))

(res 'get-signal)
((a 'set-signal!) 1)
(res 'get-signal)
((b 'set-signal!) 1)
(res 'get-signal)
(res2 'get-signal)