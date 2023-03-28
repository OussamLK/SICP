#lang racket/base
(require rackunit)

(define ns (make-base-namespace))


(define (create-environment parent)
  (define (create-binding symbol value) (cons symbol value))
  (define (get-binding-symbol b) (car b))
  (define (get-binding-value b) (cdr b))
  (let ((bindings '()))

    (define (find-binding sym)
      (define (loop rest-bindings)
        (if (null? rest-bindings)
           (if (null? parent) 'not-found ((parent 'get-binding) sym))
              (let ((head-sym (get-binding-symbol (car rest-bindings)))
                    (head-val (get-binding-value (car rest-bindings))))
                (if (eq? head-sym sym) head-val
                    (loop (cdr rest-bindings))))))
      (loop bindings))

    (define (set-binding! sym val)
      ;inefficient because it duplicates bindings that alreay exist but works
      (set! bindings (cons (create-binding sym val) bindings)))
    

    (define (dispatch m)
      (cond ((eq? m 'get-binding)
                (lambda (sym)
                   (let ((b (find-binding sym)))
                      (if (eq? b 'not-found) (error "unbound variable: " sym) b))))
            ((eq? 'set-binding! m) set-binding!)
            (else (error "unknow command" m))))
    dispatch))


(define (ev exp env)
  (define (primitive-procedure? exp) (member (car exp) '(+ - * /)))
  (define (get-value sym) (ev sym env))
  (cond ((number? exp) exp)
        ((symbol? exp) ([env 'get-binding] exp))
        ((pair? exp)
         (cond ((primitive-procedure? exp) (eval (cons (car exp)
                                                    [map get-value (cdr exp)])
                                              ns))
               ((eq? 'define (car exp)) ((env 'set-binding!) (cadr exp) (ev (caddr exp) env) ))
               ((eq? 'quote (car exp)) exp)
               ((eq? 'lambda (car exp)) (create-function (cadr exp) (caddr exp) env))
               (else (let ([function (ev (car exp) env)])
                       (if function (apply function [map get-value (cdr exp)] )
                           (error "I can not find function: " (car exp)))))
           ))
        (else (error "I dont know how to evaluate " exp))))


(define (create-function params body env)
  (define (dispatch m)
    (cond ((eq? m 'get-params) params)
          ((eq? m 'get-body) body)
          ((eq? m 'get-env) env)
          (else (error "unknow command " m))))
  dispatch)

(define (apply function params-values)
  (let ((env (create-environment (function 'get-env)))
        (body (function 'get-body))
        (params-names (function 'get-params)))
    
    (define (bind-params rest-params rest-values)
      (if (null? rest-params) 'done-binding
          (begin
             ((env 'set-binding!) (car rest-params) (ev [car rest-values] env))
             (bind-params (cdr rest-params) (cdr rest-values)))))
    (bind-params params-names params-values)

    (define (evaluate-body rest-body)
      (if (null? (cdr rest-body)) (ev (car rest-body) env)
          (begin (ev (car rest-body) env)
                 (evaluate-body (cdr rest-body)))))
    (evaluate-body body)))

;; tests

(define (setup-test-envs)
  ;;returns a tuples with (g . env)
  ;;g is the global frame
  ;;env is a child
  ;; env sees two bindings 'a: 1 and 'b: 2
   (define g (create-environment '()))
   (define env (create-environment g))
   ([g 'set-binding!] 'a 1)
   ([env 'set-binding!] 'b 2)
  (cons g env))
  
(test-begin
 "Testing the environment API"
 (define envs (setup-test-envs))
 (let ([g (car envs)]
       [env (cdr envs)])
   (check-equal? ([env 'get-binding] 'a) 1 "retrieving value of 'a")
   (check-equal? ([env 'get-binding] 'b) 2 "retrieving value of 'b")))

(test-begin
 "Testing the evaluation of primitive expressions"
 (define envs (setup-test-envs))
 (let ([g (car envs)]
       [env (cdr envs)])
   (check-equal? [ev '2 env] 2 "evaluating quoted expressions")
   (check-equal? [ev '(+ 1 2) env] 3 "evaluating (+ 1 2)")
   (check-equal? [ev '(* 1 2) env] 2 "evaluating (* 1 2)")
   (check-equal? [ev '(- 1 2) env] -1 "evaluating (- 1 2)")
   (check-equal? [ev '(/ 1 2) env] (/ 1 2) "evaluating (/ 1 2)")))

(test-begin
 "Testing the definition and evaluation of functions"
 (define envs (setup-test-envs))
 (let ([g (car envs)]
       [env (cdr envs)])
   ((env 'set-binding!) 'f1 (create-function '(a b) (list '(- (* a 2) b)) env))
   (check-equal? [ev '(f1 2 6)  env] -2 "evaluating quoted expressions")
   ))

