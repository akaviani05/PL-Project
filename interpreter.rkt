#lang racket

(require "datatypes.rkt")
(require "environment.rkt")
(require (lib "eopl.ss" "eopl"))

; Error reporting
(define (report-invalid-expression! expr) 
  (eopl:error 'invalid-expression "this expression is invalid: ~s" expr))

(define (report-invalid-statement! stmt) 
  (eopl:error 'invalid-statement "this statement is invalid: ~s" stmt))

; Helper function to compare expression values for equality
(define equal-values?
  (lambda (val1 val2)
    (cond
      ; Both are numbers
      [(and (expval? val1) (expval? val2))
       (cases expval val1
         (num-val (n1) 
          (cases expval val2
            (num-val (n2) (= n1 n2))
            (float-val (f2) (= n1 f2))
            (else #f)))
         (float-val (f1)
          (cases expval val2
            (num-val (n2) (= f1 n2))
            (float-val (f2) (= f1 f2))
            (else #f)))
         (bool-val (b1)
          (cases expval val2
            (bool-val (b2) (eq? b1 b2))
            (else #f)))
         (string-val (s1)
          (cases expval val2
            (string-val (s2) (string=? s1 s2))
            (else #f)))
         (char-val (c1)
          (cases expval val2
            (char-val (c2) (string=? c1 c2))
            (else #f))))]
      [else #f])))

; Main program evaluation - expects AST from parser
(define value-of-program
  (lambda (ast)
    (cond
      [(and (pair? ast) (eq? (car ast) 'program))
       (let ((statements (cadr ast)))
         (exec-statement-list statements (init-env)))]
      [else (report-invalid-expression! ast)])))

; Execute a list of statements and return the result of the last one
(define exec-statement-list
  (lambda (stmt-list env)
    (cond
      [(null? stmt-list) (num-val 0)]
      [(null? (cdr stmt-list)) 
       ; Last statement - return its value
       (exec-statement (car stmt-list) env)]
      [else 
       ; Execute statement and continue with updated environment
       (let ((new-env (exec-statement-for-env (car stmt-list) env)))
         (exec-statement-list (cdr stmt-list) new-env))])))

; Execute statement and return its value (for expression evaluation)
(define exec-statement
  (lambda (stmt env)
    (cond
      [(and (pair? stmt) (eq? (car stmt) 'simple-stament))
       (exec-simple-statement (cadr stmt) env)]
      [(and (pair? stmt) (eq? (car stmt) 'scope))
       ; Execute statements in scope and return last result
       (let ((statements (cadr stmt)))
         (exec-statement-list statements env))]
      [else (report-invalid-statement! stmt)])))

; Execute statement and return updated environment (for environment updates)
(define exec-statement-for-env
  (lambda (stmt env)
    (cond
      [(and (pair? stmt) (eq? (car stmt) 'simple-stament))
       (exec-simple-statement-for-env (cadr stmt) env)]
      [(and (pair? stmt) (eq? (car stmt) 'scope))
       ; Scope doesn't change outer environment
       env]
      [else env])))

; Execute simple statement and return its value
(define exec-simple-statement
  (lambda (stmt env)
    (cond
      [(and (pair? stmt) (eq? (car stmt) 'var-declaration))
       (exec-var-declaration (cadr stmt) env)]
      [(and (pair? stmt) (eq? (car stmt) 'expression))
       (value-of-expression (cadr stmt) env)]
      [else (report-invalid-statement! stmt)])))

; Execute simple statement and return updated environment
(define exec-simple-statement-for-env
  (lambda (stmt env)
    (cond
      [(and (pair? stmt) (eq? (car stmt) 'var-declaration))
       (exec-var-declaration-for-env (cadr stmt) env)]
      [(and (pair? stmt) (eq? (car stmt) 'expression))
       ; Expression statements don't change environment
       env]
      [else env])))

; Execute variable declaration and return the assigned value
(define exec-var-declaration
  (lambda (var-decl env)
    (cond
      [(and (pair? var-decl) (eq? (car var-decl) 'var-assign))
       ; var-type-name = expression
       (let* ((var-type-name (cadr var-decl))
              (init-expr (caddr var-decl))
              (val (value-of-expression init-expr env)))
         val)]
      [(and (pair? var-decl) (eq? (car var-decl) 'var-default))
       ; var-type-name (default initialization)
       (let* ((var-type-name (cadr var-decl))
              (default-val (get-default-value var-type-name)))
         default-val)]
      [else (report-invalid-statement! var-decl)])))

; Execute variable declaration and return updated environment
(define exec-var-declaration-for-env
  (lambda (var-decl env)
    (cond
      [(and (pair? var-decl) (eq? (car var-decl) 'var-assign))
       ; var-type-name = expression
       (let* ((var-type-name (cadr var-decl))
              (init-expr (caddr var-decl))
              (var-name (extract-var-name var-type-name))
              (val (value-of-expression init-expr env)))
         (extend-env var-name val env))]
      [(and (pair? var-decl) (eq? (car var-decl) 'var-default))
       ; var-type-name (default initialization)
       (let* ((var-type-name (cadr var-decl))
              (var-name (extract-var-name var-type-name))
              (default-val (get-default-value var-type-name)))
         (extend-env var-name default-val env))]
      [else env])))

; Extract variable name from var-type-name node
(define extract-var-name
  (lambda (var-type-name)
    (cond
      [(and (pair? var-type-name) (eq? (car var-type-name) 'var-type-name))
       (caddr var-type-name)] ; Third element is the ID
      [else (report-invalid-expression! var-type-name)])))

; Get default value based on type
(define get-default-value
  (lambda (var-type-name)
    (let ((var-type (cadr var-type-name)))
      (cond
        [(and (pair? var-type) (eq? (car var-type) 'int-t)) (num-val 0)]
        [(and (pair? var-type) (eq? (car var-type) 'float-t)) (float-val 0.0)]
        [(and (pair? var-type) (eq? (car var-type) 'bool-t)) (bool-val #f)]
        [(and (pair? var-type) (eq? (car var-type) 'string-t)) (string-val "")]
        [(and (pair? var-type) (eq? (car var-type) 'char-t)) (char-val "")]
        [else (num-val 0)]))))

; Expression evaluation - handles the nested exp structure from parser
(define value-of-expression
  (lambda (expr env)
    (cond
      ; Handle the nested exp6 -> exp5 -> exp4 -> exp3 -> exp2 -> exp1 -> exp0 structure
      [(and (pair? expr) (eq? (car expr) 'exp6)) (value-of-expression (cadr expr) env)]
      [(and (pair? expr) (eq? (car expr) 'exp5)) (value-of-expression (cadr expr) env)]
      [(and (pair? expr) (eq? (car expr) 'exp4)) (value-of-expression (cadr expr) env)]
      [(and (pair? expr) (eq? (car expr) 'exp3)) (value-of-expression (cadr expr) env)]
      [(and (pair? expr) (eq? (car expr) 'exp2)) (value-of-expression (cadr expr) env)]
      [(and (pair? expr) (eq? (car expr) 'exp1)) (value-of-expression (cadr expr) env)]
      [(and (pair? expr) (eq? (car expr) 'exp0)) (value-of-expression (cadr expr) env)]
      
      ; Handle arithmetic operations (binary)
      [(and (pair? expr) (eq? (car expr) '+))
       (let ((val1 (expval->num (value-of-expression (cadr expr) env)))
             (val2 (expval->num (value-of-expression (caddr expr) env))))
         (num-val (+ val1 val2)))]
      
      [(and (pair? expr) (eq? (car expr) '-))
       (let ((val1 (expval->num (value-of-expression (cadr expr) env)))
             (val2 (expval->num (value-of-expression (caddr expr) env))))
         (num-val (- val1 val2)))]
      
      [(and (pair? expr) (eq? (car expr) '*))
       (let ((val1 (expval->num (value-of-expression (cadr expr) env)))
             (val2 (expval->num (value-of-expression (caddr expr) env))))
         (num-val (* val1 val2)))]
      
      [(and (pair? expr) (eq? (car expr) '/))
       (let ((val1 (expval->num (value-of-expression (cadr expr) env)))
             (val2 (expval->num (value-of-expression (caddr expr) env))))
         (if (= val2 0)
             (eopl:error 'division-by-zero "division by zero")
             (num-val (quotient val1 val2))))]
      
      [(and (pair? expr) (eq? (car expr) '%))
       (let ((val1 (expval->num (value-of-expression (cadr expr) env)))
             (val2 (expval->num (value-of-expression (caddr expr) env))))
         (if (= val2 0)
             (eopl:error 'division-by-zero "modulo by zero")
             (num-val (remainder val1 val2))))]
      
      ; Handle unary operations
      [(and (pair? expr) (eq? (car expr) '!))
       (let ((val (expval->bool (value-of-expression (cadr expr) env))))
         (bool-val (not val)))]
      
      [(and (pair? expr) (eq? (car expr) '~))
       (let ((val (expval->num (value-of-expression (cadr expr) env))))
         (num-val (bitwise-not val)))]
      
      ; Handle comparison operations (Op4)
      [(and (pair? expr) (eq? (car expr) '<=))
       (let ((val1 (expval->num (value-of-expression (cadr expr) env)))
             (val2 (expval->num (value-of-expression (caddr expr) env))))
         (bool-val (<= val1 val2)))]
      
      [(and (pair? expr) (eq? (car expr) '>=))
       (let ((val1 (expval->num (value-of-expression (cadr expr) env)))
             (val2 (expval->num (value-of-expression (caddr expr) env))))
         (bool-val (>= val1 val2)))]
      
      [(and (pair? expr) (eq? (car expr) '<))
       (let ((val1 (expval->num (value-of-expression (cadr expr) env)))
             (val2 (expval->num (value-of-expression (caddr expr) env))))
         (bool-val (< val1 val2)))]
      
      [(and (pair? expr) (eq? (car expr) '>))
       (let ((val1 (expval->num (value-of-expression (cadr expr) env)))
             (val2 (expval->num (value-of-expression (caddr expr) env))))
         (bool-val (> val1 val2)))]
      
      [(and (pair? expr) (eq? (car expr) '==))
       (let ((val1 (value-of-expression (cadr expr) env))
             (val2 (value-of-expression (caddr expr) env)))
         (bool-val (equal-values? val1 val2)))]
      
      [(and (pair? expr) (eq? (car expr) '!=))
       (let ((val1 (value-of-expression (cadr expr) env))
             (val2 (value-of-expression (caddr expr) env)))
         (bool-val (not (equal-values? val1 val2))))]
      
      ; Handle bitwise operations (Op5)
      [(and (pair? expr) (eq? (car expr) '&))
       (let ((val1 (expval->num (value-of-expression (cadr expr) env)))
             (val2 (expval->num (value-of-expression (caddr expr) env))))
         (num-val (bitwise-and val1 val2)))]
      
      [(and (pair? expr) (eq? (car expr) '^))
       (let ((val1 (expval->num (value-of-expression (cadr expr) env)))
             (val2 (expval->num (value-of-expression (caddr expr) env))))
         (num-val (bitwise-xor val1 val2)))]
      
      [(and (pair? expr) (eq? (car expr) 'orop))
       (let ((val1 (expval->num (value-of-expression (cadr expr) env)))
             (val2 (expval->num (value-of-expression (caddr expr) env))))
         (num-val (bitwise-ior val1 val2)))]
      
      ; Handle logical operations (Op6 and Op7)
      [(and (pair? expr) (eq? (car expr) '&&))
       (let ((val1 (expval->bool (value-of-expression (cadr expr) env))))
         (if val1
             (let ((val2 (expval->bool (value-of-expression (caddr expr) env))))
               (bool-val val2))
             (bool-val #f)))]
      
      [(and (pair? expr) (eq? (car expr) '||))
       (let ((val1 (expval->bool (value-of-expression (cadr expr) env))))
         (if val1
             (bool-val #t)
             (let ((val2 (expval->bool (value-of-expression (caddr expr) env))))
               (bool-val val2))))]
      
      ; Handle atoms
      [(and (pair? expr) (eq? (car expr) 'atom))
       (value-of-atom (cadr expr) env)]
      
      ; Handle parenthesized expressions
      [(and (pair? expr) (eq? (car expr) 'paren))
       (value-of-expression (cadr expr) env)]
      
      [else (report-invalid-expression! expr)])))

; Atom evaluation
(define value-of-atom
  (lambda (atom env)
    (cond
      [(and (pair? atom) (eq? (car atom) 'value-atom))
       (value-of-value (cadr atom))]
      [(and (pair? atom) (eq? (car atom) 'var-name-atom))
       (apply-env (cadr atom) env)]
      [(and (pair? atom) (eq? (car atom) 'assignment-atom))
       (value-of-assignment (cadr atom) env)]
      [else (report-invalid-expression! atom)])))

; Value evaluation
(define value-of-value
  (lambda (val)
    (cond
      [(and (pair? val) (eq? (car val) 'int-val)) (num-val (cadr val))]
      [(and (pair? val) (eq? (car val) 'float-val)) (float-val (cadr val))]
      [(and (pair? val) (eq? (car val) 'bool-val)) 
       (bool-val (if (string=? (cadr val) "true") #t #f))]
      [(and (pair? val) (eq? (car val) 'str-val)) (string-val (cadr val))]
      [(and (pair? val) (eq? (car val) 'char-val)) (char-val (cadr val))]
      [else (report-invalid-expression! val)])))

; Assignment evaluation
(define value-of-assignment
  (lambda (assignment env)
    (cond
      [(and (pair? assignment) (eq? (car assignment) 'assignment))
       (let* ((var-name (cadr assignment))
              (expr (caddr assignment))
              (val (value-of-expression expr env)))
         val)] ; For now, just return the value
      [else (report-invalid-expression! assignment)])))

(provide (all-defined-out))