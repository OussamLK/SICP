#lang racket/base
(require rackunit)
(require racket/set)
(require "data-directed-dispatch.rkt")

(define (boolean v)
  (if v #t #f))

(define (attach-logger f)
  (define (temp . args)
       (map display (list "calling function " f " on \n\t" args "\n \t result was \n \t" (apply f args) "\n"))
       (apply f args))
  temp
  )

(define (register-generic-expression type-name evaluator)
  ((data-dispatcher 'register-type) type-name)
  ((data-dispatcher 'register-function) type-name 'evaluator evaluator))

(define (known-generic-expression? exp)
  (boolean (and (pair? exp)
                (not (null? exp))
                ((data-dispatcher 'find-type) (get-tag exp)))))

(define (evaluate-generic-expression exp env)
  (let ((type (get-tag exp)))
    (if (not (known-generic-expression? exp)) (error "The expression has a type I do not know: " type)
        (let ((evaluator ((data-dispatcher 'get-function) type 'evaluator)))
          ((cdr evaluator) exp env)))))
          
(define ns (make-base-namespace))
(define nil '())

(define (tagged-list? exp tag) (and (pair? exp) (eq? tag (car exp))))
(define (parameter-values exp env) (map (lambda (v) (ev v env)) (cdr exp)))
(define (get-tag exp) (car exp))
(define (expression-parameters exp) (cdr exp))

(define (not-eq? v w) (not (eq? v w)))
(define (primitive-procedure? procedure) [set-member? (seteq + - / * = < >) procedure])
(define (variable? exp) (symbol? exp))

(define (self-evaluating? exp) (or (number? exp) [set-member? (seteq #t #f) exp])) 

(define (quoted? exp) (tagged-list? exp 'quote))
(define (text-of-quotation quotation) (cadr quotation))

(define (definition? exp) (tagged-list? exp 'define))
(define (definition-symbol def) (cadr def))
(define (definition-value def) (caddr def))
(define (eval-definition definition env)
  ;;Shouldn't I be using list-of-values instead of evaluating on the fly?
  (if (pair? (definition-symbol definition))
      (let* ((function-declaration (definition-symbol definition))
             (function-name (car function-declaration))
             (function-parameters (cdr function-declaration))
             (function-body (cddr definition))
             (new-function (cons 'lambda (cons function-parameters function-body))))
         [(env 'set-binding!) function-name (ev new-function env)]) 
             
      [(env 'set-binding!) (definition-symbol definition) (ev (definition-value definition) env)]))

(register-generic-expression 'define eval-definition)

(define (assignment? exp) (tagged-list? exp 'set!))
(define (assignment-symbol assignment) (cadr assignment))
(define (assignment-value assignment) (caddr assignment))
(define (eval-assignment assignment env)
  ((env 'make-assignment!)
     (assignment-symbol assignment)
     (ev (assignment-value assignment) env)))
  


(define (lambda? exp) (tagged-list? exp 'lambda))
(define (lambda-parameters lambda-expression) (cadr lambda-expression))
(define (lambda-body lambda-expression) (cddr lambda-expression))

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

(register-generic-expression 'if eval-if)

(define (application? exp) (pair? exp))
(define (no-operands? exp) (null? exp))
(define (operands application) (cdr application))
(define (first-operand exp) (car (operands exp)))
(define (rest-operands exp) (cdr (operands exp)))
(define (operator application) (car application))
(define (evaluate-function exp env)
  (define (get-value var) (ev var env))
  (let ([function (ev (operator exp) env)])
                                   (if function (apply_ function [map get-value (cdr exp)])
                                     (error "I can not find function: " (operator exp)))))

(define (call? exp) (tagged-list? exp 'call))
(define call-function cdr)

(define (begin? exp) (tagged-list? exp 'begin))
(define (last-expression? exp) (null? (cdr exp)))
(define (make-begin seq) (cons 'begin seq))
(define (begin-actions exp) (cdr exp))
(define (eval-sequence seq env)
  (cond ((null? seq) (error "Can not evaluate null sequence"))
        ((last-expression? seq) (ev (car seq) env))
        (else (ev (car seq) env)
              (eval-sequence (cdr seq) env))))
(define (eval-begin exp env) (eval-sequence (begin-actions exp) env))
(define (seq->begin seq) (make-begin seq))

(register-generic-expression 'begin eval-begin)

(define (cond? exp) (tagged-list? exp 'cond))
(define (clauses exp) (cdr exp))
(define (first-clause clauses) (car clauses))
(define (rest-clauses clauses) (cdr clauses))
(define (clause-predicate clause) (car clause))
(define (clause-actions clause) (cdr clause))
(define (else-clause? clause) (tagged-list? clause 'else))
(define (else-action clause) (cadr clause))
(define (arrow-clause? clause) (eq? (cadr clause) '=>))
(define (arrow-clause-function clause) (caddr clause))

(define (not-implemented! . args) (error "unimplemented error"))
(define (clauses->if clauses)
  (if (null? clauses) #f
      (let ((clause (car clauses)))
           (cond ((arrow-clause? clause) [list 'if (clause-predicate clause)
                                                     (list (arrow-clause-function clause) (clause-predicate clause))
                                                     (clauses->if (rest-clauses clauses))
                                                ])
                 ((else-clause? clause) (seq->begin (clause-actions clause)))
                 (else (list 'if (clause-predicate (first-clause clauses))
                                     (seq->begin (clause-actions clause))
                                     (clauses->if (rest-clauses clauses))
                     ))))))

(define (cond->if exp) (clauses->if (clauses exp)))
(define (eval-cond exp env) (ev (cond->if exp) env))


(define (or? exp) (tagged-list? 'or exp))
(define (eval-or exp env)
    (define (loop parameters)
      (cond ((null? parameters) #f)
            ((ev (car parameters) env) #t)
            (else (loop (cdr parameters)))))
    (loop (expression-parameters exp)))
(register-generic-expression 'or eval-or)

(define (and? exp) (tagged-list? 'and exp))
(define (eval-and exp env)
  (define (loop parameters)
    (cond ((null? parameters) #t)
          ((not (ev (car parameters) env)) #f)
          (else (loop (cdr parameters)))))
  (loop (expression-parameters exp)))
(register-generic-expression 'and eval-and)

(define (let? exp) (tagged-list? 'let exp))
(define let-bindings cadr)
(define let-body cddr)
(define (let->lambda exp env)
  (define (get-value var) (ev var env))
  (let ((binding-symbols (map car (let-bindings exp)))
        (binding-values (map get-value (map cadr (let-bindings exp))))
        (body (let-body exp)))
    (cons (cons 'lambda (cons binding-symbols body)) binding-values)))
(define (eval-let exp env) (ev (let->lambda exp env) env))
(register-generic-expression 'let eval-let)



(define (create-environment parent)
  (define (create-binding symbol value) (cons symbol value))
  (define (get-binding-symbol b) (car b))
  (define (get-binding-value b) (cdr b))
  (let ((bindings '()))

    (define (binding-in-frame? sym)
      (let ([frame-symbols (map get-binding-symbol bindings)])
            (member sym frame-symbols)))
    
    (define (find-binding-env sym env)
      ;finds the environment where sym is bound
      ;if unbound, returns nil
      (cond ((null? env) nil)
            ([(env 'binding-in-frame?) sym] env)
            (else (find-binding-env sym (env 'get-parent)))))
      
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

    (define (make-assignment! symb val)
      (let ((env (find-binding-env symb self)))
        (if env ((env 'set-binding!) symb val)
            (error "You are trying to assign to an unbound variable: " symb))))

    (define (self m)
      (cond ((eq? m 'get-binding)
                (lambda (sym)
                   (let ((b (find-binding sym)))
                      (if (eq? b 'not-found) (error "unbound variable: " sym) b))))
            ((eq? 'set-binding! m) set-binding!)
            ((eq? 'get-parent m) parent)
            ((eq? 'binding-in-frame? m) binding-in-frame?)
            ((eq? 'find-binding-env m) find-binding-env)
            ((eq? 'make-assignment! m) make-assignment!)
            (else (error "unknow command" m))))
    self))



(define (ev exp env)  
  (define (get-value sym) (ev sym env))
  (cond ((known-generic-expression? exp) (evaluate-generic-expression exp env))
        ((call? exp) (evaluate-function (call-function exp) env))
        ((self-evaluating? exp) exp)
        ((variable? exp) ([env 'get-binding] exp))
        ;((definition? exp) (eval-definition exp env))
        ((assignment? exp) (eval-assignment exp env))
        ((quoted? exp) (text-of-quotation exp))
        ;((if? exp) (eval-if exp env))
        ;((begin? exp) (eval-begin exp env))
        ((lambda? exp) (make-procedure (lambda-parameters exp) (lambda-body exp) env))
        ((cond? exp) (eval-cond exp env))
        ((application? exp) (evaluate-function exp env))
        (else (error "I do not know how to evaluate: " exp ))))


(define (evaluate-sequence sequence env)
   (if (last-expression? sequence) (ev (car sequence) env)
       (begin (ev (car sequence) env)
              (evaluate-sequence (cdr sequence) env))))


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
   (for-each (lambda (op) ([g 'set-binding!] op (eval op ns))) '(+ - / * = > <))
  (cons g env))
  
(test-begin
 "Testing the environment API"
 (define envs (setup-test-envs))
 (let ([g (car envs)]
       [env (cdr envs)])
   (check-equal? ([env 'get-binding] 'a) 1 "retrieving value of 'a")
   (check-equal? ([env 'get-binding] 'b) 2 "retrieving value of 'b")
   (check-equal? ([env 'binding-in-frame?] 'a) #f "looking for a binding that is not in the frame")
   (check-not-equal? ([env 'binding-in-frame?] 'b) #f "looking for a binding that is in the frame")
   (check-equal? ([env 'find-binding-env] 'a env) g "a is bound in the global frame")
   (check-equal? ([env 'find-binding-env] 'b env) env "b is bound in env frame")
   (check-equal? ([env 'find-binding-env] 'c env) nil "looking for an unbound symbol")

   ((env 'make-assignment!) 'a -1)
   ((env 'make-assignment!) 'b -2)
   (check-equal? ([env 'get-binding] 'a) -1 "setting a global variable")
    ))

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
 "Testing the definitions and assignments"
 (define envs (setup-test-envs))
 (let [(g (car envs))
       (env (cdr envs))]
   (ev '(define x -1) env)
   (check-equal? (ev 'x env) -1 "checking that definitions work")
   (ev '(set! x -2) env)
   (check-equal? (ev 'x env) -2 "checking assignments")
   (ev '(set! a (+ 1 x)) env)
   
   (check-equal? (ev 'a env) -1 "checking assignments in parent frame")
   ;;function definitions
   (ev '(define (f x y) (* x y)) env)
   (check-equal? (ev '(f 2 3) env) 6 "checking defined function")
   (ev '(define (g x y) (define z 8) (- (f x y) z)) env)
   (check-equal? (ev '(g 2 3) env) -2 "testing evaluation of a complex function")
   (ev '(define (lexical-scoping) (define (add-one x) (+ x 1)) (add-one 3)) env)
   (check-equal? (ev '(lexical-scoping) env) 4 "testing function with internal definitions")
   ))

(test-begin
 "Testing the evaluation of begin expressions"
 (define envs (setup-test-envs))
 (let ([g (car envs)]
       [env (cdr envs)])
   (define begin-exp (make-begin '( (define x_ -4) x_)))
   (check-equal? (ev begin-exp env) -4 "checking a that begin works")
   (define seq '( (define y_ -3) y_))
   (check-equal? (ev (seq->begin seq) env) -3 "Testing seq->begin")
   ))

(test-begin
 "Testing the definition and evaluation of functions"
 (define envs (setup-test-envs))
 (let ([g (car envs)]
       [env (cdr envs)])
   ((env 'set-binding!) 'f1 (make-procedure '(a b) (list '(- (* a 2) b)) env))
   (check-equal? [ev '(f1 2 6)  env] -2 "evaluating quoted expressions")
   ((env 'set-binding!) 'f2 (make-procedure '() (list 0 1) env))
   (check-equal? (ev '(f2) env) 1 "evaluating a procedure with no operands")
   (check-equal? (ev '(call f2) env) (ev '(f2) env) "testing call")
   (check-equal? (ev '((lambda (x) x) 0) env) 0 "checking the call of lambda functions on the fly")
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

(test-begin
 "Testing the cond expression"
 (define envs (setup-test-envs))
 (let [(g (car envs))
       (env (cdr envs))]
   (define cond1 '(cond ((< x 0) (- x))
                        (else x)))
   (ev '(define x -3) env)
   (ev '(define y -3) env)
   (check-equal? (ev cond1 env) 3 "checking cond with absolute value")
   (ev '(define x 2) env)
   (check-equal? (ev cond1 env) 2 "checking abs with positive value")
   (define cond2 '(cond ((> y 0) 0) (else y)))
   (check-equal? (ev cond2 env) -3 "checking a cond with an else clause")
   (define cond-arrow '[cond (0 => (lambda (x) x))])
   (check-equal? (ev cond-arrow env) 0 "Checking for the => special form cond clause")
   
   ))

(test-begin
 "testing the and and or expression"
 (define envs (setup-test-envs))
 (let [(g (car envs))
       (env (cdr envs))]
   (define cond1 '(cond ((< x 0) (- x))
                        (else x)))
   (ev '(define t #t) env)
   (ev '(define f #f) env)
   (check-equal? (ev '(or t f) env) #t "checking that or returns true when there is a true value")
   (check-equal? (ev '(or f f) env) #f "checking that or returns true when there is a true value")
   (check-equal? (ev '(or f f f t) env) #t "checking or when the last value is true")
   (check-equal? (ev '(and f f f t) env) #f "checking and when the last value is true")
   (check-equal? (ev '(and t t t t f) env) #f "checking and when the last value is false")

   ))

(test-begin
 "testing the let expression"
 (define envs (setup-test-envs))
 (let [(g (car envs))
       (env (cdr envs))]
   (define cond1 '(cond ((< x 0) (- x))
                        (else x)))
   (define let-exp '(let ((x 3) (y 2)) (define z 7) (* z y x)))
   (check-equal? (ev let-exp g) 42 "checking that let expressions work as expected")
   (ev '(define z 2) env)
   (check-equal? (ev '(let ((x 1)) (+ x z)) env) 3 "checking evaluation of variables inside let expressions")
   ))
