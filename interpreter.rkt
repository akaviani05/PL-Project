#lang racket

(require "datatypes.rkt")
(require "environment.rkt")
(require (lib "eopl.ss" "eopl"))

; Export main functions
(provide value-of-program)

; Error reporting
(define (report-invalid-expression! expr) 
  (eopl:error 'invalid-expression "this expression is invalid: ~s" expr))

(define (report-invalid-statement! stmt) 
  (eopl:error 'invalid-statement "this statement is invalid: ~s" stmt))

; Helper functions for mixed-type arithmetic
(define (extract-numeric-value val)
  "Extract numeric value from either int or float expval"
  (cases expval val
    (num-val (num) num)
    (float-val (fl) fl)
    (else (eopl:error 'type-error "Expected numeric value, got ~s" val))))

(define (create-numeric-result result expval1 expval2)
  "Create result expval based on operand types - if any operand is float, result is float"
  (cond
    [(or (is-float-expval? expval1) (is-float-expval? expval2))
     (float-val (exact->inexact result))]
    [else (num-val (inexact->exact result))]))

(define (is-float-expval? val)
  "Check if expval is a float type"
  (cases expval val
    (float-val (fl) #t)
    (else #f)))

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
            (else #f)))
         (break-val ()
          (cases expval val2
            (break-val () #t)
            (else #f)))
         (continue-val ()
          (cases expval val2
            (continue-val () #t)
            (else #f)))
         (return-val (v1)
          (cases expval val2
            (return-val (v2) (equal-values? v1 v2))
            (else #f)))
         (function-val (p1 b1 e1)
          (cases expval val2
            (function-val (p2 b2 e2) #t) ; Functions are equal if they exist (simplified)
            (else #f)))
         (list-val (lst1)
          (cases expval val2
            (list-val (lst2) (equal-lists? lst1 lst2))
            (else #f))))]
      [else #f])))

; Helper function to compare lists for equality
(define equal-lists?
  (lambda (lst1 lst2)
    (cond
      [(and (null? lst1) (null? lst2)) #t]
      [(or (null? lst1) (null? lst2)) #f]
      [else 
       (and (equal-values? (car lst1) (car lst2))
            (equal-lists? (cdr lst1) (cdr lst2)))])))

; Helper function to extract parameter names from parser output
(define extract-parameter-names
  (lambda (param-list)
    (cond
      [(null? param-list) '()]
      [(pair? param-list)
       ; Process each parameter in the list
       (map (lambda (param)
              (cond
                [(and (pair? param) (eq? (car param) 'var-type-name))
                 ; Extract the ID from (var-type-name var-type ID)
                 (caddr param)]
                [else param]))
            param-list)]
      [else '()])))

; Helper function to extract argument expressions, handling parser sequences
(define extract-argument-expressions
  (lambda (arg-list)
    (cond
      [(null? arg-list) '()]
      [(pair? arg-list) arg-list]  ; The parser now gives us actual lists
      [else (list arg-list)])))

; Main program evaluation - expects AST from parser
(define value-of-program
  (lambda (ast)
    (cond
      [(and (pair? ast) (eq? (car ast) 'program))
       (let ((statements (cadr ast)))
         (let ((result-env-pair (exec-statement-list-unified statements (init-env))))
           (car result-env-pair)))]
      [(and (pair? ast) (eq? (car ast) 'expression))  ; Handle standalone expressions
       (value-of-expression (cadr ast) (init-env))]
      [else (report-invalid-expression! ast)])))

; === UNIFIED EXECUTION SYSTEM ===
; All functions return (cons result environment)

; Unified execution of statement list that returns both result and environment
(define exec-statement-list-unified
  (lambda (stmt-list env)
    (cond
      [(null? stmt-list) (cons (num-val 0) env)]
      [(null? (cdr stmt-list)) 
       ; Last statement - execute once and return both result and environment
       (exec-statement-unified (car stmt-list) env)]
      [else 
       ; Execute current statement and check for break/continue
       (let* ((result-env-pair (exec-statement-unified (car stmt-list) env))
              (current-result (car result-env-pair))
              (new-env (cdr result-env-pair)))
         ; Check if current statement returned break or continue
         (cases expval current-result
           (break-val () result-env-pair)  ; Propagate break up
           (continue-val () result-env-pair)  ; Propagate continue up
           (else 
             ; Continue with next statement
             (exec-statement-list-unified (cdr stmt-list) new-env))))])))

; Unified statement execution
(define exec-statement-unified
  (lambda (stmt env)
    (cond
      [(and (pair? stmt) (eq? (car stmt) 'simple-stament))
       (exec-simple-statement-unified (cadr stmt) env)]
      [(and (pair? stmt) (eq? (car stmt) 'scope))
       ; Execute statements in scope and return result and environment
       (let ((statements (cadr stmt)))
         (exec-statement-list-unified statements env))]
      [(and (pair? stmt) (eq? (car stmt) 'if-statement))
       (exec-if-statement-unified (cadr stmt) env)]
      [(and (pair? stmt) (eq? (car stmt) 'while-statement))
       (exec-while-statement-unified (cadr stmt) env)]
      [else (cons (report-invalid-statement! stmt) env)])))

; Unified simple statement execution
(define exec-simple-statement-unified
  (lambda (stmt env)
    (cond
      [(and (pair? stmt) (eq? (car stmt) 'var-declaration))
       (exec-var-declaration-unified (cadr stmt) env)]
      [(and (pair? stmt) (eq? (car stmt) 'func-declaration))
       (exec-function-declaration-unified (cadr stmt) env)]
      [(and (pair? stmt) (eq? (car stmt) 'expression))
       (let ((expr (cadr stmt)))
         (if (is-assignment-expression? expr)
             ; Assignment - execute once and return both result and new env
             (let ((result (value-of-expression expr env))
                   (new-env (exec-assignment-for-env expr env)))
               (cons result new-env))
             ; Check if it's a predefined statement that might update environment
             (if (is-predefined-expression? expr)
                 ; Predefined statement - might update environment
                 (value-of-predefined-statement-unified-from-expr expr env)
                 ; Non-assignment, non-predefined - return result and same environment
                 (let ((result (value-of-expression expr env)))
                   (cons result env)))))]
      [(and (pair? stmt) (eq? (car stmt) 'break-statement))
       (cons (break-val) env)]
      [(and (pair? stmt) (eq? (car stmt) 'continue-statement))
       (cons (continue-val) env)]
      [(and (pair? stmt) (eq? (car stmt) 'return-statement))
       (exec-return-statement-unified (cadr stmt) env)]
      [else (cons (report-invalid-statement! stmt) env)])))

; Helper to check if expression contains predefined statement
(define is-predefined-expression?
  (lambda (expr)
    (cond
      [(and (pair? expr) (member (car expr) '(exp6 exp5 exp4 exp3 exp2 exp1 exp0)))
       (is-predefined-expression? (cadr expr))]
      [(and (pair? expr) (eq? (car expr) 'atom))
       (let ((atom (cadr expr)))
         (and (pair? atom) (eq? (car atom) 'predefined-atom)))]
      [else #f])))

; Helper to execute predefined statement from expression context
(define value-of-predefined-statement-unified-from-expr
  (lambda (expr env)
    (cond
      [(and (pair? expr) (member (car expr) '(exp6 exp5 exp4 exp3 exp2 exp1 exp0)))
       (value-of-predefined-statement-unified-from-expr (cadr expr) env)]
      [(and (pair? expr) (eq? (car expr) 'atom))
       (let ((atom (cadr expr)))
         (if (and (pair? atom) (eq? (car atom) 'predefined-atom))
             (value-of-predefined-statement-unified (cadr atom) env)
             (cons (value-of-expression expr env) env)))]
      [else (cons (value-of-expression expr env) env)])))

; Unified variable declaration execution
(define exec-var-declaration-unified
  (lambda (var-decl env)
    (cond
      [(and (pair? var-decl) (eq? (car var-decl) 'var-assign))
       ; var-type-name = expression
       (let* ((var-type-name (cadr var-decl))
              (init-expr (caddr var-decl))
              (var-name (extract-var-name var-type-name))
              (val (value-of-expression init-expr env))
              (new-env (extend-env var-name val env)))
         (cons val new-env))]
      [(and (pair? var-decl) (eq? (car var-decl) 'var-default))
       ; var-type-name (default initialization)
       (let* ((var-type-name (cadr var-decl))
              (var-name (extract-var-name var-type-name))
              (default-val (get-default-value var-type-name))
              (new-env (extend-env var-name default-val env)))
         (cons default-val new-env))]
      [else (cons (report-invalid-statement! var-decl) env)])))

; Unified function declaration execution  
(define exec-function-declaration-unified
  (lambda (func-decl env)
    (cond
      [(and (pair? func-decl) (eq? (car func-decl) 'function-declaration))
       ; Function declaration structure: (function-declaration return-type func-name scope params)
       (let* ((return-type (cadr func-decl))
              (func-name (caddr func-decl))
              (scope (cadddr func-decl))
              (raw-params (if (null? (cddddr func-decl)) '() (car (cddddr func-decl))))
              (params (extract-parameter-names raw-params)))
         ; For recursion: create function with environment that includes itself
         ; We'll use a letrec-style approach where the function is available in its own closure
         (let* ((extended-env (extend-env func-name (num-val 0) env))  ; temporary placeholder
                (func-val (function-val params scope extended-env)))
           ; Now update the environment so the function can see itself
           (let ((final-env (extend-env func-name func-val env)))
             (cons func-val final-env))))]
      [else (cons (report-invalid-statement! func-decl) env)])))

; Unified if statement execution
(define exec-if-statement-unified
  (lambda (if-stmt env)
    (cond
      ; Simple if: (if condition scope)
      [(and (pair? if-stmt) (eq? (car if-stmt) 'if))
       (let* ((condition (cadr if-stmt))
              (then-scope (caddr if-stmt))
              (condition-val (expval->bool (value-of-expression condition env))))
         (if condition-val
             (exec-statement-unified then-scope env)
             (cons (num-val 0) env)))] ; Return 0 and unchanged env if condition is false
      
      ; if-else: (if condition then-scope else-scope)
      [(and (pair? if-stmt) (eq? (car if-stmt) 'if-else))
       (let* ((condition (cadr if-stmt))
              (then-scope (caddr if-stmt))
              (else-scope (cadddr if-stmt))
              (condition-val (expval->bool (value-of-expression condition env))))
         (if condition-val
             (exec-statement-unified then-scope env)
             (exec-statement-unified else-scope env)))]
      
      ; if-elseif: (if condition then-scope elseif-statement)
      [(and (pair? if-stmt) (eq? (car if-stmt) 'if-elseif))
       (let* ((condition (cadr if-stmt))
              (then-scope (caddr if-stmt))
              (elseif-stmt (cadddr if-stmt))
              (condition-val (expval->bool (value-of-expression condition env))))
         (if condition-val
             (exec-statement-unified then-scope env)
             (exec-if-statement-unified elseif-stmt env)))]
      
      [else (cons (report-invalid-statement! if-stmt) env)])))

; Unified while statement execution
(define exec-while-statement-unified
  (lambda (while-stmt env)
    (cond
      ; while: (while condition scope)
      [(and (pair? while-stmt) (eq? (car while-stmt) 'while))
       (let* ((condition (cadr while-stmt))
              (body-scope (caddr while-stmt)))
         (exec-while-loop-unified condition body-scope env))]
      
      [else (cons (report-invalid-statement! while-stmt) env)])))

; Unified while loop execution
(define exec-while-loop-unified
  (lambda (condition body-scope env)
    (let ((condition-val (expval->bool (value-of-expression condition env))))
      (if condition-val
          ; Execute body once and get both result and new environment
          (let* ((result-env-pair (exec-statement-unified body-scope env))
                 (body-result (car result-env-pair))
                 (new-env (cdr result-env-pair)))
            ; Check if body returned break or continue
            (cases expval body-result
              (break-val () (cons (num-val 0) new-env))  ; Break out of loop
              (continue-val () (exec-while-loop-unified condition body-scope new-env))  ; Continue to next iteration
              (else 
                ; Continue looping with updated environment
                (exec-while-loop-unified condition body-scope new-env))))
          (cons (num-val 0) env))))) ; Return 0 when loop ends

; === BACKWARD COMPATIBILITY FUNCTIONS ===
; These call the unified functions but only return the requested part

; Execute a list of statements and return the result of the last one
(define exec-statement-list
  (lambda (stmt-list env)
    (let ((result-env-pair (exec-statement-list-unified stmt-list env)))
      (car result-env-pair))))

; Execute a list of statements and return the final environment
(define exec-statement-list-for-env
  (lambda (stmt-list env)
    (let ((result-env-pair (exec-statement-list-unified stmt-list env)))
      (cdr result-env-pair))))

; Execute statement and return its value (for expression evaluation)
(define exec-statement
  (lambda (stmt env)
    (let ((result-env-pair (exec-statement-unified stmt env)))
      (car result-env-pair))))

; Execute statement and return updated environment (for environment updates)
(define exec-statement-for-env
  (lambda (stmt env)
    (let ((result-env-pair (exec-statement-unified stmt env)))
      (cdr result-env-pair))))

; Execute simple statement and return its value
(define exec-simple-statement
  (lambda (stmt env)
    (let ((result-env-pair (exec-simple-statement-unified stmt env)))
      (car result-env-pair))))

; Execute simple statement and return updated environment
(define exec-simple-statement-for-env
  (lambda (stmt env)
    (let ((result-env-pair (exec-simple-statement-unified stmt env)))
      (cdr result-env-pair))))

; Execute variable declaration and return the assigned value
(define exec-var-declaration
  (lambda (var-decl env)
    (let ((result-env-pair (exec-var-declaration-unified var-decl env)))
      (car result-env-pair))))

; Execute variable declaration and return updated environment
(define exec-var-declaration-for-env
  (lambda (var-decl env)
    (let ((result-env-pair (exec-var-declaration-unified var-decl env)))
      (cdr result-env-pair))))

; === HELPER FUNCTIONS ===

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
        [(and (pair? var-type) (eq? (car var-type) 'list-t)) (list-val '())]
        [else (num-val 0)]))))

; Helper function to check if an expression contains an assignment
(define is-assignment-expression?
  (lambda (expr)
    (cond
      ; Handle the nested exp structure
      [(and (pair? expr) (member (car expr) '(exp6 exp5 exp4 exp3 exp2 exp1 exp0)))
       (is-assignment-expression? (cadr expr))]
      
      ; Handle atoms
      [(and (pair? expr) (eq? (car expr) 'atom))
       (let ((atom (cadr expr)))
         (and (pair? atom) (eq? (car atom) 'assignment-atom)))]
      
      [else #f])))

; Execute assignment for environment update
(define exec-assignment-for-env
  (lambda (expr env)
    (cond
      ; Handle the nested exp structure
      [(and (pair? expr) (member (car expr) '(exp6 exp5 exp4 exp3 exp2 exp1 exp0)))
       (exec-assignment-for-env (cadr expr) env)]
      
      ; Handle atoms
      [(and (pair? expr) (eq? (car expr) 'atom))
       (let ((atom (cadr expr)))
         (cond
           [(and (pair? atom) (eq? (car atom) 'assignment-atom))
            (let* ((assignment (cadr atom))
                   (var-name (cadr assignment))
                   (assign-expr (caddr assignment))
                   (val (value-of-expression assign-expr env)))
              (extend-env var-name val env))]
           [else env]))]
      
      [else env])))

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
      
      ; Handle arithmetic operations (binary) - supports mixed int/float arithmetic
      [(and (pair? expr) (eq? (car expr) '+))
       (let ((expval1 (value-of-expression (cadr expr) env))
             (expval2 (value-of-expression (caddr expr) env)))
         (let ((val1 (extract-numeric-value expval1))
               (val2 (extract-numeric-value expval2)))
           (create-numeric-result (+ val1 val2) expval1 expval2)))]
      
      [(and (pair? expr) (eq? (car expr) '-))
       (let ((expval1 (value-of-expression (cadr expr) env))
             (expval2 (value-of-expression (caddr expr) env)))
         (let ((val1 (extract-numeric-value expval1))
               (val2 (extract-numeric-value expval2)))
           (create-numeric-result (- val1 val2) expval1 expval2)))]
      
      [(and (pair? expr) (eq? (car expr) '*))
       (let ((expval1 (value-of-expression (cadr expr) env))
             (expval2 (value-of-expression (caddr expr) env)))
         (let ((val1 (extract-numeric-value expval1))
               (val2 (extract-numeric-value expval2)))
           (create-numeric-result (* val1 val2) expval1 expval2)))]
      
      [(and (pair? expr) (eq? (car expr) '/))
       (let ((expval1 (value-of-expression (cadr expr) env))
             (expval2 (value-of-expression (caddr expr) env)))
         (let ((val1 (extract-numeric-value expval1))
               (val2 (extract-numeric-value expval2)))
           (if (= val2 0)
               (eopl:error 'division-by-zero "division by zero")
               ; Check if both operands are integers (no floats involved)
               (if (and (not (is-float-expval? expval1)) (not (is-float-expval? expval2)))
                   ; Integer division - use quotient for integer result
                   (num-val (quotient (inexact->exact val1) (inexact->exact val2)))
                   ; Float division - use regular division
                   (create-numeric-result (/ val1 val2) expval1 expval2)))))]

      [(and (pair? expr) (eq? (car expr) '%))
       (let ((expval1 (value-of-expression (cadr expr) env))
             (expval2 (value-of-expression (caddr expr) env)))
         (let ((val1 (extract-numeric-value expval1))
               (val2 (extract-numeric-value expval2)))
           (if (= val2 0)
               (eopl:error 'division-by-zero "modulo by zero")
               (num-val (remainder (inexact->exact (floor val1)) (inexact->exact (floor val2)))))))]
      
      ; Handle unary operations
      [(and (pair? expr) (eq? (car expr) 'unary-minus))
       (let ((val (extract-numeric-value (value-of-expression (cadr expr) env))))
         (if (integer? val)
             (num-val (- val))
             (float-val (- val))))]
      
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
      [(and (pair? atom) (eq? (car atom) 'predefined-atom))
       (value-of-predefined-statement (cadr atom) env)]
      [(and (pair? atom) (eq? (car atom) 'function-atom))
       (value-of-function-call (cadr atom) env)]
      [else (report-invalid-expression! atom)])))

; Predefined statement evaluation with environment updates
(define value-of-predefined-statement-unified
  (lambda (predefined-stmt env)
    (cond
      [(and (pair? predefined-stmt) (eq? (car predefined-stmt) 'print-statement))
       ; Evaluate the expression and print it
       (let ((val (value-of-expression (cadr predefined-stmt) env)))
         (display (expval->string-for-print val))
         (newline)
         (cons val env))] ; Return value and unchanged environment
      [(and (pair? predefined-stmt) (eq? (car predefined-stmt) 'input-statement))
       (value-of-input-statement-unified (cadr predefined-stmt) env)]
      [(and (pair? predefined-stmt) (eq? (car predefined-stmt) 'get-statement))
       (let ((val (value-of-get-statement (cadr predefined-stmt) (caddr predefined-stmt) env)))
         (cons val env))]
      [(and (pair? predefined-stmt) (eq? (car predefined-stmt) 'set-statement))
       (value-of-set-statement-unified (cadr predefined-stmt) (caddr predefined-stmt) (cadddr predefined-stmt) env)]
      [(and (pair? predefined-stmt) (eq? (car predefined-stmt) 'push-statement))
       (value-of-push-statement-unified (cadr predefined-stmt) (caddr predefined-stmt) env)]
      [(and (pair? predefined-stmt) (eq? (car predefined-stmt) 'pop-statement))
       (value-of-pop-statement-unified (cadr predefined-stmt) env)]
      [(and (pair? predefined-stmt) (eq? (car predefined-stmt) 'size-statement))
       (let ((list-expval (apply-env (cadr predefined-stmt) env)))
         (cases expval list-expval
           (list-val (lst)
             (let ((size-val (num-val (length lst))))
               (cons size-val env)))
           (else (eopl:error 'type-error "$size expects a list, got ~s" list-expval))))]
      [(and (pair? predefined-stmt) (eq? (car predefined-stmt) 'tocharlist-statement))
       (let ((val (value-of-expression (cadr predefined-stmt) env)))
         (cases expval val
           (string-val (str)
             (let* ((char-list (map (lambda (i)
                                      (char-val (string (string-ref str i))))
                                    (range (string-length str))))
                    (result (list-val char-list)))
               (cons result env)))
           (else (eopl:error 'type-error "$tocharlist expects a string, got ~s" val))))]
      [else (cons (report-invalid-expression! predefined-stmt) env)])))

; Predefined statement evaluation (backward compatibility)
(define value-of-predefined-statement
  (lambda (predefined-stmt env)
    (let ((result-env-pair (value-of-predefined-statement-unified predefined-stmt env)))
      (car result-env-pair))))

; Input statement - reads a value and assigns it to a variable
(define value-of-input-statement
  (lambda (var-name env)
    (display "Input: ")
    (let ((input-str (read-line)))
      ; Try to parse as number first, then as string
      (let ((val (cond
                   [(string->number input-str) (num-val (string->number input-str))]
                   [(string=? input-str "true") (bool-val #t)]
                   [(string=? input-str "false") (bool-val #f)]
                   [else (string-val input-str)])))
        val))))

; Input statement - reads a value and assigns it to a variable (unified version)
(define value-of-input-statement-unified
  (lambda (var-name env)
    (display "Input: ")
    (let ((input-str (read-line)))
      ; Try to parse as number first, then as string
      (let ((val (cond
                   [(string->number input-str) (num-val (string->number input-str))]
                   [(string=? input-str "true") (bool-val #t)]
                   [(string=? input-str "false") (bool-val #f)]
                   [else (string-val input-str)])))
        ; Update the environment with the new value and return both value and updated environment
        (let ((new-env (extend-env var-name val env)))
          (cons val new-env))))))

; Get statement - retrieves element from list at index
(define value-of-get-statement
  (lambda (var-name index-expr env)
    (let ((list-expval (apply-env var-name env))
          (index-val (value-of-expression index-expr env)))
      (cases expval list-expval
        (list-val (lst)
         (let ((index (expval->num index-val)))
           (if (and (>= index 0) (< index (length lst)))
               (list-ref lst index)
               (eopl:error 'list-index-error "list index out of range: ~s" index))))
        (else (eopl:error 'type-error "expected list, got ~s" list-expval))))))

; Set statement - sets element in list at index (backward compatibility)
(define value-of-set-statement
  (lambda (var-name index-expr value-expr env)
    (let ((result-env-pair (value-of-set-statement-unified var-name index-expr value-expr env)))
      (car result-env-pair))))

; Push statement - adds element to end of list (backward compatibility)
(define value-of-push-statement
  (lambda (var-name value-expr env)
    (let ((result-env-pair (value-of-push-statement-unified var-name value-expr env)))
      (car result-env-pair))))

; Pop statement - removes and returns last element from list (backward compatibility)
(define value-of-pop-statement
  (lambda (var-name env)
    (let ((result-env-pair (value-of-pop-statement-unified var-name env)))
      (car result-env-pair))))

; Set statement - sets element in list at index and updates environment
(define value-of-set-statement-unified
  (lambda (var-name index-expr value-expr env)
    (let ((list-expval (apply-env var-name env))
          (index-val (value-of-expression index-expr env))
          (new-val (value-of-expression value-expr env)))
      (cases expval list-expval
        (list-val (lst)
         (let ((index (expval->num index-val)))
           (if (and (>= index 0) (< index (length lst)))
               (let* ((new-lst (list-set lst index new-val))
                      (new-list-val (list-val new-lst)))
                 (cons new-list-val (extend-env var-name new-list-val env)))
               (eopl:error 'list-index-error "list index out of range: ~s" index))))
        (else (eopl:error 'type-error "expected list, got ~s" list-expval))))))

; Push statement - adds element to end of list and updates environment
(define value-of-push-statement-unified
  (lambda (var-name value-expr env)
    (let ((list-expval (apply-env var-name env))
          (new-val (value-of-expression value-expr env)))
      (cases expval list-expval
        (list-val (lst)
         (let* ((new-lst (append lst (list new-val)))
                (new-list-val (list-val new-lst)))
           (cons new-list-val (extend-env var-name new-list-val env))))
        (else (eopl:error 'type-error "expected list, got ~s" list-expval))))))

; Pop statement - removes and returns last element from list and updates environment
(define value-of-pop-statement-unified
  (lambda (var-name env)
    (let ((list-expval (apply-env var-name env)))
      (cases expval list-expval
        (list-val (lst)
         (if (null? lst)
             (eopl:error 'list-empty-error "cannot pop from empty list")
             (let* ((last-elem (car (reverse lst)))
                    (new-lst (reverse (cdr (reverse lst))))
                    (new-list-val (list-val new-lst)))
               (cons new-list-val (extend-env var-name new-list-val env)))))
        (else (eopl:error 'type-error "expected list, got ~s" list-expval))))))

; Helper function to set element in list at index
(define list-set
  (lambda (lst index new-val)
    (cond
      [(= index 0) (cons new-val (cdr lst))]
      [else (cons (car lst) (list-set (cdr lst) (- index 1) new-val))])))

; Helper function to convert expval to string for printing
(define expval->string-for-print
  (lambda (val)
    (cases expval val
      (num-val (num) (number->string num))
      (float-val (fl) (number->string fl))
      (bool-val (bool) (if bool "true" "false"))
      (string-val (str) 
        ; Remove surrounding quotes if they exist
        (let ((clean-str (if (and (> (string-length str) 1)
                                 (eq? (string-ref str 0) #\")
                                 (eq? (string-ref str (- (string-length str) 1)) #\"))
                            (substring str 1 (- (string-length str) 1))
                            str)))
          clean-str))
      (char-val (ch) 
        ; Remove surrounding quotes if they exist
        (let ((clean-ch (if (and (> (string-length ch) 1)
                                (eq? (string-ref ch 0) #\')
                                (eq? (string-ref ch (- (string-length ch) 1)) #\'))
                           (substring ch 1 (- (string-length ch) 1))
                           ch)))
          clean-ch))
      (list-val (lst) 
        (string-append "[" 
          (string-join (map expval->string-for-print lst) ", ") 
          "]"))
      [else "unknown"])))

; Value evaluation
(define value-of-value
  (lambda (val)
    (cond
      [(and (pair? val) (eq? (car val) 'int-val)) (num-val (cadr val))]
      [(and (pair? val) (eq? (car val) 'float-val)) (float-val (cadr val))]
      [(and (pair? val) (eq? (car val) 'bool-val)) 
       (bool-val (if (string=? (cadr val) "true") #t #f))]
      [(and (pair? val) (eq? (car val) 'str-val)) 
       (let ((str (cadr val)))
         (if (and (> (string-length str) 1)
                  (char=? (string-ref str 0) #\")
                  (char=? (string-ref str (- (string-length str) 1)) #\"))
             (string-val (substring str 1 (- (string-length str) 1)))
             (string-val str)))]
      [(and (pair? val) (eq? (car val) 'char-val)) 
       (let ((char-str (cadr val)))
         (if (and (> (string-length char-str) 2)
                  (char=? (string-ref char-str 0) #\')
                  (char=? (string-ref char-str (- (string-length char-str) 1)) #\'))
             (char-val (substring char-str 1 (- (string-length char-str) 1)))
             (char-val char-str)))]
      [(and (pair? val) (eq? (car val) 'lst-val)) (value-of-list-literal (cadr val))]
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

; === FUNCTION EXECUTION FUNCTIONS ===

; Unified return statement execution
(define exec-return-statement-unified
  (lambda (return-stmt env)
    (cond
      ; Return without value: (return-statement)
      [(null? return-stmt)
       (cons (return-val (num-val 0)) env)]
      ; Return with nested return-statement: (return-statement expression)
      [(and (pair? return-stmt) (eq? (car return-stmt) 'return-statement))
       (exec-return-statement-unified (cdr return-stmt) env)]
      ; Return with expression: (expression)
      [(pair? return-stmt)
       (let ((expr (car return-stmt)))
         (let ((val (value-of-expression expr env)))
           (cons (return-val val) env)))]
      [else (cons (report-invalid-statement! return-stmt) env)])))

; Execute function call and return result
(define value-of-function-call
  (lambda (func-call env)
    (cond
      [(and (pair? func-call) (eq? (car func-call) 'function-call))
       ; Function call structure: (function-call func-name args)
       (let* ((func-name (cadr func-call))
              (raw-args (caddr func-call))
              (args (extract-argument-expressions raw-args))
              (func-val (apply-env func-name env)))
         (cases expval func-val
           (function-val (params body closure-env)
             ; Evaluate arguments
             (let ((arg-vals (map (lambda (arg) (value-of-expression arg env)) args)))
               ; Create new environment with parameter bindings
               ; Important: make sure the function can call itself by including it in the parameter environment
               (let* ((func-env (extend-env func-name func-val closure-env))
                      (param-env (bind-parameters params arg-vals func-env)))
                 ; Execute function body
                 (let ((result-env-pair (exec-statement-unified body param-env)))
                   (let ((result (car result-env-pair)))
                     ; Check if result is a return value
                     (cases expval result
                       (return-val (val) val)  ; Extract return value
                       (else result)))))))     ; Return result as-is
           (else (report-invalid-expression! (list "not a function:" func-name)))))]
      [else (report-invalid-expression! func-call)])))

; Existing extract-parameter-names function

; List literal evaluation
(define value-of-list-literal
  (lambda (value-seq)
    (cond
      [(null? value-seq) (list-val '())]
      [(pair? value-seq)
       (let ((values (map value-of-value value-seq)))
         (list-val values))]
      [else (list-val (list (value-of-value value-seq)))])))

; Helper function to join strings with separator
(define string-join
  (lambda (str-list separator)
    (cond
      [(null? str-list) ""]
      [(null? (cdr str-list)) (car str-list)]
      [else (string-append (car str-list) separator (string-join (cdr str-list) separator))])))

(define bind-parameters
  (lambda (params args env)
    (cond
      [(and (null? params) (null? args)) env]
      [(or (null? params) (null? args)) 
       (eopl:error 'bind-parameters "parameter/argument count mismatch")]
      [else
       ; Extract parameter name from var-type-name structure
       (let* ((param (car params))
              (param-name (if (and (pair? param) (eq? (car param) 'var-type-name))
                             (caddr param)  ; Extract ID from (var-type-name var-type ID)
                             param))        ; If not var-type-name, use as-is
              (arg-val (car args))
              (new-env (extend-env param-name arg-val env)))
         (bind-parameters (cdr params) (cdr args) new-env))])))