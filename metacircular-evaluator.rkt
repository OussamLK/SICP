#lang racket/base
(require rackunit)
(require racket/set)

(define ns (make-base-namespace))

(define (tagged-list? exp tag) (eq? tag (car exp)))
(define (not-eq? v w) (not (eq? v w)))
(define (primitive-procedure? procedure) [set-member? (seteq + - / * =) procedure])
(define (variable? exp) (symbol? exp))

(define (self-evaluating? exp) (or (number? exp) [set-member? (seteq #t #f) exp])) 

(define (quoted? exp) (tagged-list? exp 'quote))
(define (text-of-quotation quotation) (cadr quotation))

(define (definition? exp) (tagged-list? exp 'define))
(define (eval-definition definition env) [(env 'set-binding!) (cadr definition) (ev (caddr definition) env)])

(define (lambda? exp) (tagged-list? exp 'lambda))
(define (lambda-parameters lambda-expression) (cadr lambda-expression))
(define (lambda-body lambda-expression) (caddr lambda-expression))

(define (if? exp) (tagged-list? exp 'if))
(define (if-predicate if-expression) (cadr if-expression))
(define (if-consequent if-expression) (caddr if-expression))
(define (if-alternative if-expression) (cadddr if-expression))
(define (true? exp env) (not-eq? (ev exp env) #f))
(define (eval-if if-expression env)
  (let [(predicate (if-predicate if-expression))
        (consequent (if-consequent if-expression))
        (alternative (if-alternative if-expression))]
    (if (true? predicate env) (ev consequent env) (ev alternative env))))

(define (application? exp) (pair? exp))
(define (operands application) (cdr application))
(define (operator application) (car application))


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
  (define (get-value sym) (ev sym env))
  (cond ((self-evaluating? exp) exp)
        ((variable? exp) ([env 'get-binding] exp))
        ((definition? exp) (eval-definition exp env))
        ((quoted? exp) (text-of-quotation exp))
        ((if? exp) (eval-if exp env))
        ((lambda? exp) (make-procedure (lambda-parameters exp) (lambda-body exp) env))
        ((application? exp) (let ([function (ev (operator exp) env)])
                                   (if function (apply_ function [map get-value (cdr exp)])
                                     (error "I can not find function: " (operator exp)))))
        (else (error "I do not know how to evaluate: " exp ))))


(define (evaluate-sequence sequence env)
   (if (null? (cdr sequence)) (ev (car sequence) env)
       (begin (ev (car sequence) env)
              (evaluate-sequence (cdr sequence)))))


(define (make-procedure params body env)
  (define (dispatch m)
    (cond ((eq? m 'get-params) params)
          ((eq? m 'get-body) body)
          ((eq? m 'get-env) env)
          (else (error "unknow command " m))))
  dispatch)

(define (apply_ function params-values)
  (if (primitive-procedure? function) (apply function params-values)
      (let ((env (create-environment (function 'get-env)))
            (body (function 'get-body))
            (params-names (function 'get-params)))
        ;binding the parameters in the new environment
        (define (bind symb val) ((env 'set-binding!) symb val))
        (for-each bind params-names params-values)

        (evaluate-sequence body env))))

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
   (for-each (lambda (op) ([g 'set-binding!] op (eval op ns))) '(+ - / * =))
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
   ((env 'set-binding!) 'f1 (make-procedure '(a b) (list '(- (* a 2) b)) env))
   (check-equal? [ev '(f1 2 6)  env] -2 "evaluating quoted expressions")
   ))

(test-begin
 "Testing the if evaluation"
 (define envs (setup-test-envs))
 (let [(g (car envs))
       (env (cdr envs))]
   (check-equal? [ev '(if #t a b) env] 1 "evaluating a true predicate")
   (check-equal? [ev '(if 1 a b) env] 1 "evaluating an int predicate")
   (check-equal? [ev '(if #f a b) env] 2 "evaluating a false predicate")
   (check-equal? [ev '(if (= 1 2) a b) env] 2 "evaluating a false compound predicate")))
