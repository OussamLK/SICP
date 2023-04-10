#lang racket


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

(define [evaluate-operands exp env]
  (define (eval-ops operands env)
      ;(map display (list "looping for operands" operands)) 
      (if (null? operands) '() (cons [ev (car operands) env] [eval-ops (cdr operands) env])))
  (cons (car exp) (eval-ops (cdr exp) env)))


(define (ev exp env)
  (define (atomic-procedure? exp) (member (car exp) '(+ - * /)))
  (cond ((number? exp) exp)
        ((symbol? exp) ([env 'get-binding] exp))
        ((pair? exp)
         (cond ((atomic-procedure? exp) (eval [evaluate-operands exp env] ns))
               ((eq? 'define (car exp)) ((env 'set-binding!) (cadr exp) (ev (caddr exp) env) ))
               ((eq? 'quote (car exp)) exp)
               ((eq? 'lambda (car exp)) (create-function (cadr exp) (caddr exp) env))
               (else (error "I do not know how to evaluate " (car exp)))
           ))
        (else (error "I dont know how to evaluate " exp))))


(define (create-function params body env)
  (define (dispatch m)
    (cond ((eq? m 'get-params) params)
          ((eq? m 'get-body) body)
          ((eq? m 'get-env) env)
          (else (error "unknow command " m))))
  dispatch)

(define (evaluate-function function params-values)
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

;;tests
(define g (create-environment '()))
(define env (create-environment g))
([env 'set-binding!] 'a 1)
([env 'set-binding!] 'b 2)
;(ev '(+ a b) env)

(define add (create-function '(a b) (list '(- (* a 10) b)) env))
(evaluate-function add (list 1 2))