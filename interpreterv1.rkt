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
            (else #f))))]
      [else #f])))

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
             ; Non-assignment - return result and same environment
             (let ((result (value-of-expression expr env)))
               (cons result env))))]
      [(and (pair? stmt) (eq? (car stmt) 'break-statement))
       (cons (break-val) env)]
      [(and (pair? stmt) (eq? (car stmt) 'continue-statement))
       (cons (continue-val) env)]
      [(and (pair? stmt) (eq? (car stmt) 'return-statement))
       (exec-return-statement-unified (cadr stmt) env)]
      [else (cons (report-invalid-statement! stmt) env)])))

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
         ; Simple approach: create function with current env, 
         ; recursion will be handled in function call by looking up the function name
         (let* ((func-val (function-val params scope env))
                (new-env (extend-env func-name func-val env)))
           (cons func-val new-env)))]
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
      [(and (pair? atom) (eq? (car atom) 'predefined-atom))
       (value-of-predefined-statement (cadr atom) env)]
      [(and (pair? atom) (eq? (car atom) 'function-atom))
       (value-of-function-call (cadr atom) env)]
      [else (report-invalid-expression! atom)])))

; Predefined statement evaluation
(define value-of-predefined-statement
  (lambda (predefined-stmt env)
    (cond
      [(and (pair? predefined-stmt) (eq? (car predefined-stmt) 'print-statement))
       ; Evaluate the expression and print it
       (let ((val (value-of-expression (cadr predefined-stmt) env)))
         (display (expval->string-for-print val))
         (newline)
         val)] ; Return the value that was printed
      [else (report-invalid-expression! predefined-stmt)])))

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
      [else "unknown"])))

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
               (let ((param-env (bind-parameters params arg-vals closure-env)))
                 ; Execute function body
                 (let ((result-env-pair (exec-statement-unified body param-env)))
                   (let ((result (car result-env-pair)))
                     ; Check if result is a return value
                     (cases expval result
                       (return-val (val) val)  ; Extract return value
                       (else result)))))))     ; Return result as-is
           (else (report-invalid-expression! (list "not a function:" func-name)))))]
      [else (report-invalid-expression! func-call)])))

; Helper function to bind parameters to arguments
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

; Helper function to resolve parser-generated sequences like (append $1 (list $3))
; This handles the case where parser generates symbolic append expressions
(define resolve-parser-sequence
  (lambda (seq-expr actual-values)
    (cond
      [(null? seq-expr) '()]
      [(and (pair? seq-expr) (eq? (car seq-expr) 'list))
       ; (list $1) -> resolve $1 to actual value
       (if (eq? (cadr seq-expr) '$1)
           (list (car actual-values))
           (list (cadr seq-expr)))]
      [(and (pair? seq-expr) (eq? (car seq-expr) 'append))
       ; (append $1 (list $3)) -> resolve both parts
       (let ((first-part (resolve-parser-sequence (cadr seq-expr) actual-values))
             (second-part (resolve-parser-sequence (caddr seq-expr) actual-values)))
         (append first-part second-part))]
      [else (list seq-expr)])))

; Improved parameter extraction that handles the actual parameter structures
(define extract-parameter-names-improved
  (lambda (param-expr)
    (cond
      [(null? param-expr) '()]
      ; Handle direct var-type-name
      [(and (pair? param-expr) (eq? (car param-expr) 'var-type-name))
       (list (caddr param-expr))]  ; Extract the name part
      ; Handle list of var-type-name
      [(and (pair? param-expr) (eq? (car param-expr) 'list))
       (let ((param (cadr param-expr)))
         (extract-parameter-names-improved param))]
      ; Handle quoted expressions - this is the key fix
      [(and (pair? param-expr) (eq? (car param-expr) 'quote))
       ; Skip the quote and process the inner expression
       (extract-parameter-names-improved (cadr param-expr))]
      ; Handle append expressions with symbols $1, $3 etc.
      [(and (pair? param-expr) (eq? (car param-expr) 'append))
       ; For now, return empty list and handle this at function call time
       ; This is a parser artifact that needs special handling
       '()]
      [else '()])))

; Existing extract-parameter-names function
