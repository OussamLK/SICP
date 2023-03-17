#lang racket

(define (create-binding symbol value) (cons symbol value))
(define (get-binding-symbol b) (car b))
(define (get-binding-value b) (cdr b))



(define (create-environment parent)
  (let ((bindings '()))
    (define (find-binding sym)
      (define (loop rest-bindings)
        (if (null? rest-bindings)
           (if (null? parent) 'not-found ((parent 'get-binding) sym))
              (let ((head-sym (get-binding-symbol (car bindings)))
                    (head-val (get-binding-value (car bindings))))
                (if (eq? head-sym sym) head-val
                    (loop (cdr bindings))))))
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

;;tests
(define g (create-environment '()))
(define env (create-environment g))

((g 'set-binding!) 'x 0)
;((g 'get-binding) 'x)
;((env 'get-binding) 'x)
((env 'set-binding!) 'y 1)
;((env 'get-binding) 'y)
;((g 'get-binding) 'y)


(define (ev exp env)
  (cond ((number? exp) exp)
        ((symbol? exp) ((env 'get-binding) exp))
        ((pair? exp)
         (cond ((eq? 'define (car exp)) ((env 'set-binding!) (cadr exp) (ev (caddr exp) env) ))
               ((eq? 'quote (car exp)) exp)
           ))
        (else (error "I dont know how to evaluate " exp))))

(ev '(define x 3) env)
(ev 'x env)