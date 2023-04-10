#lang racket/base
(require rackunit)

;; I need an object with the following interface
;;    add a new type
;;    register a function for a type
;;    given a type and a function, return the right function for the type

(define (make-function name function) (cons name function))
(define (function-name function) (car function))
(define (function-self function) (cdr function))

(define (make-type type-name)
  (let ([functions '()])
    (define (register-function name function)
      (let ((new-function (make-function name function)))
        (set! functions (cons new-function functions))))
    (define (get-function name)
      (let ((match-list (member name functions (lambda (name f) (eq? (function-name f) name)))))
        (and match-list (car match-list))))

    (define (self m)
      (cond ((eq? m 'type-name) type-name)
            ((eq? m 'register-function) (lambda (name function) (register-function name function)))
            ((eq? m 'get-function) (lambda (function-name) (get-function function-name)))
            (else (error "I do not have a method " m))))
    self))


    



(define (make-data-dispatcher)
  (let [(types '())]
    
        
    
    (define (register-type type-name)
      (let ((new-type (make-type type-name)))
        (set! types (cons new-type types))
        new-type))

    (define (register-function type-name function-name function)
      (let ((type-instance (find-type type-name)))
        (if type-instance
            ((type-instance 'register-function) function-name function)
            (error "Cannot register function for unknown type"))))

    (define (get-function type-name function-name)
      (let ((type-instance (find-type type-name)))
        (if type-instance
            ((type-instance 'get-function) function-name)
            (error "Cannot get function for unknown type"))))

    (define (find-type type-name)
      (define (not-null? lst) (not (null? lst))) 
      (let ((match-list (filter (lambda (type) (eq? (type 'type-name) type-name)) types)))
        (and (not-null? match-list) (car match-list))))
    
    (define (self method)
      (cond ((eq? method 'register-type) (lambda (type-name) (register-type type-name)))
            ((eq? method 'register-function) (lambda (type-name function-name function) (register-function type-name function-name function))) 
            ((eq? method 'get-function) (lambda (type function-name) (get-function type function-name)))
            ((eq? method 'find-type) (lambda (type-name) (find-type type-name)))
            (else (error "DataDispatcher has no method " method))))
  self))

(define data-dispatcher (make-data-dispatcher))
(provide data-dispatcher)

(test-case
 "Testing functions"
 (define f1 (make-function 'add (lambda (a b) (+ a b))))
 (check-equal? (function-name f1) 'add "checking functiona name getter")
 (check-equal? ((function-self f1) 2 3) 5 "checking the function-self getter"))

(test-case
 "testing type (generated using chatGPT)"

 (define test-fn (lambda (x) (* 2 x)))
 
 ;; Test case 2: Check if the type is created successfully
(check-equal? ((make-type 'my-type) 'type-name) 'my-type)

;; Test case 3: Check if registering a function works
(define my-type-instance (make-type 'test-type))
((my-type-instance 'register-function) 'double test-fn)

;; Test case 4: Check if getting a function works
(check-equal? (function-name ((my-type-instance 'get-function) 'double)) 'double)

;; Test case 5: Check if getting the function's implementation works
(check-equal? (function-self ((my-type-instance 'get-function) 'double)) test-fn)

;; Test case 6: Check if the function's implementation executes correctly
(define retrieved-function (function-self ((my-type-instance 'get-function) 'double)))
(check-equal? (retrieved-function 3) 6)

;; Test case 7: Check if getting a non-existent function returns #f
(check-equal? ((my-type-instance 'get-function) 'non_existent_function) #f)

)

(test-case
 "Testing DataDispatcher"
 (define data-dispatcher (make-data-dispatcher))
 (define test-fn (lambda (x) (* 2 x)))
 
 ;; Test case 1: Successfully registering a new type
 ((data-dispatcher 'register-type) 'test-type)

 ;; Test case 2: Successfully registering a function for the type
 ((data-dispatcher 'register-function) 'test-type 'double test-fn)

 ;; Test case 3: Successfully getting a function for the type
 (check-equal? (function-name ((data-dispatcher 'get-function) 'test-type 'double)) 'double)

 ;; Test case 4: Successfully executing the function retrieved for the type
 (define retrieved-function (function-self ((data-dispatcher 'get-function) 'test-type 'double)))
 (check-equal? (retrieved-function 3) 6)

 ;; Test case 5: Registering a function for an unknown type should throw an error
 (check-exn exn:fail?
            (lambda () ((data-dispatcher 'register-function) 'unknown-type 'double test-fn))
            "Should throw error when trying to register function for unknown type")

 ;; Test case 6: Getting a function for an unknown type should throw an error
 (check-exn exn:fail?
            (lambda () ((data-dispatcher 'get-function) 'unknown-type 'double))
            "Should throw error when trying to get function for unknown type")

 ;; Test case 7: Getting a non-existent function for a known type should return #f
 (check-equal? ((data-dispatcher 'get-function) 'test-type 'non_existent_function) #f)

 ;; Test case 8: Trying to find a non existing type should return #f
 (check-equal? ((data-dispatcher 'find-type) 'non-existant-type) #f "Testing that finding a non existant type returns #f")
 
 )