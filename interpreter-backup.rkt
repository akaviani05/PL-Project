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
            (else #f)))
         (break-val ()
          (cases expval val2
            (break-val () #t)
            (else #f)))
         (continue-val ()
          (cases expval val2
            (continue-val () #t)
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
       ; Execute current statement and check for break/continue
       (let* ((current-result (exec-statement (car stmt-list) env))
              (new-env (exec-statement-for-env (car stmt-list) env)))
         ; Check if current statement returned break or continue
         (cases expval current-result
           (break-val () current-result)  ; Propagate break up
           (continue-val () current-result)  ; Propagate continue up
           (else 
             ; Continue with next statement
             (exec-statement-list (cdr stmt-list) new-env))))])))

; Execute a list of statements and return the final environment
(define exec-statement-list-for-env
  (lambda (stmt-list env)
    (cond
      [(null? stmt-list) env]
      [else 
       ; Execute current statement and check for break/continue
       (let* ((current-result (exec-statement (car stmt-list) env))
              (new-env (exec-statement-for-env (car stmt-list) env)))
         ; Check if current statement returned break or continue
         (cases expval current-result
           (break-val () new-env)  ; Stop processing and return environment
           (continue-val () new-env)  ; Stop processing and return environment
           (else 
             ; Continue with next statement
             (exec-statement-list-for-env (cdr stmt-list) new-env))))])))

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
      [(and (pair? stmt) (eq? (car stmt) 'if-statement))
       (exec-if-statement (cadr stmt) env)]
      [(and (pair? stmt) (eq? (car stmt) 'while-statement))
       (exec-while-statement (cadr stmt) env)]
      [else (report-invalid-statement! stmt)])))

; Execute statement and return updated environment (for environment updates)
(define exec-statement-for-env
  (lambda (stmt env)
    (cond
      [(and (pair? stmt) (eq? (car stmt) 'simple-stament))
       (exec-simple-statement-for-env (cadr stmt) env)]
      [(and (pair? stmt) (eq? (car stmt) 'scope))
       ; Execute statements in scope and return updated environment
       (let ((statements (cadr stmt)))
         (exec-statement-list-for-env statements env))]
      [(and (pair? stmt) (eq? (car stmt) 'if-statement))
       (exec-if-statement-for-env (cadr stmt) env)]
      [(and (pair? stmt) (eq? (car stmt) 'while-statement))
       (exec-while-statement-for-env (cadr stmt) env)]
      [else env])))

; Execute simple statement and return its value
(define exec-simple-statement
  (lambda (stmt env)
    (cond
      [(and (pair? stmt) (eq? (car stmt) 'var-declaration))
       (exec-var-declaration (cadr stmt) env)]
      [(and (pair? stmt) (eq? (car stmt) 'expression))
       (value-of-expression (cadr stmt) env)]
      [(and (pair? stmt) (eq? (car stmt) 'break-statement))
       (break-val)]  ; Special value to indicate break
      [(and (pair? stmt) (eq? (car stmt) 'continue-statement))
       (continue-val)]  ; Special value to indicate continue
      [else (report-invalid-statement! stmt)])))

; Execute simple statement and return updated environment
(define exec-simple-statement-for-env
  (lambda (stmt env)
    (cond
      [(and (pair? stmt) (eq? (car stmt) 'var-declaration))
       (exec-var-declaration-for-env (cadr stmt) env)]
      [(and (pair? stmt) (eq? (car stmt) 'expression))
       ; Check if the expression is an assignment and update environment accordingly
       (let ((expr (cadr stmt)))
         (if (is-assignment-expression? expr)
             (exec-assignment-for-env expr env)
             env))] ; Non-assignment expressions don't change environment
      [(and (pair? stmt) (eq? (car stmt) 'break-statement))
       env]  ; break doesn't change environment
      [(and (pair? stmt) (eq? (car stmt) 'continue-statement))
       env]  ; continue doesn't change environment
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

; Execute if statement and return its value
(define exec-if-statement
  (lambda (if-stmt env)
    (cond
      ; Simple if: (if condition scope)
      [(and (pair? if-stmt) (eq? (car if-stmt) 'if))
       (let* ((condition (cadr if-stmt))
              (then-scope (caddr if-stmt))
              (condition-val (expval->bool (value-of-expression condition env))))
         (if condition-val
             (exec-statement then-scope env)
             (num-val 0)))] ; Return 0 if condition is false
      
      ; if-else: (if condition then-scope else-scope)
      [(and (pair? if-stmt) (eq? (car if-stmt) 'if-else))
       (let* ((condition (cadr if-stmt))
              (then-scope (caddr if-stmt))
              (else-scope (cadddr if-stmt))
              (condition-val (expval->bool (value-of-expression condition env))))
         (if condition-val
             (exec-statement then-scope env)
             (exec-statement else-scope env)))]
      
      ; if-elseif: (if condition then-scope elseif-statement)
      [(and (pair? if-stmt) (eq? (car if-stmt) 'if-elseif))
       (let* ((condition (cadr if-stmt))
              (then-scope (caddr if-stmt))
              (elseif-stmt (cadddr if-stmt))
              (condition-val (expval->bool (value-of-expression condition env))))
         (if condition-val
             (exec-statement then-scope env)
             (exec-if-statement elseif-stmt env)))]
      
      [else (report-invalid-statement! if-stmt)])))

; Execute if statement and return updated environment
(define exec-if-statement-for-env
  (lambda (if-stmt env)
    (cond
      ; Simple if: (if condition scope)
      [(and (pair? if-stmt) (eq? (car if-stmt) 'if))
       (let* ((condition (cadr if-stmt))
              (then-scope (caddr if-stmt))
              (condition-val (expval->bool (value-of-expression condition env))))
         (if condition-val
             (exec-statement-for-env then-scope env)
             env))] ; No change to environment if condition is false
      
      ; if-else: (if condition then-scope else-scope)
      [(and (pair? if-stmt) (eq? (car if-stmt) 'if-else))
       (let* ((condition (cadr if-stmt))
              (then-scope (caddr if-stmt))
              (else-scope (cadddr if-stmt))
              (condition-val (expval->bool (value-of-expression condition env))))
         (if condition-val
             (exec-statement-for-env then-scope env)
             (exec-statement-for-env else-scope env)))]
      
      ; if-elseif: (if condition then-scope elseif-statement)
      [(and (pair? if-stmt) (eq? (car if-stmt) 'if-elseif))
       (let* ((condition (cadr if-stmt))
              (then-scope (caddr if-stmt))
              (elseif-stmt (cadddr if-stmt))
              (condition-val (expval->bool (value-of-expression condition env))))
         (if condition-val
             (exec-statement-for-env then-scope env)
             (exec-if-statement-for-env elseif-stmt env)))]
      
      [else env])))

; Execute while statement and return its value
(define exec-while-statement
  (lambda (while-stmt env)
    (cond
      ; while: (while condition scope)
      [(and (pair? while-stmt) (eq? (car while-stmt) 'while))
       (let* ((condition (cadr while-stmt))
              (body-scope (caddr while-stmt)))
         (exec-while-loop condition body-scope env))]
      
      [else (report-invalid-statement! while-stmt)])))

; Execute while statement and return updated environment
(define exec-while-statement-for-env
  (lambda (while-stmt env)
    (cond
      ; while: (while condition scope)
      [(and (pair? while-stmt) (eq? (car while-stmt) 'while))
       (let* ((condition (cadr while-stmt))
              (body-scope (caddr while-stmt)))
         (exec-while-loop-for-env condition body-scope env))]
      
      [else env])))

; Helper function to execute while loop and return last value
(define exec-while-loop
  (lambda (condition body-scope env)
    (let ((condition-val (expval->bool (value-of-expression condition env))))
      (if condition-val
          ; Execute body once and get both result and new environment
          (let* ((result-and-env (exec-statement-with-env body-scope env))
                 (body-result (car result-and-env))
                 (new-env (cdr result-and-env)))
            ; Check if body returned break or continue
            (cases expval body-result
              (break-val () (num-val 0))  ; Break out of loop
              (continue-val () (exec-while-loop condition body-scope new-env))  ; Continue to next iteration
              (else 
                ; Continue looping with updated environment
                (exec-while-loop condition body-scope new-env))))
          (num-val 0))))) ; Return 0 when loop ends

; Helper function to execute while loop and return final environment
(define exec-while-loop-for-env
  (lambda (condition body-scope env)
    (let ((condition-val (expval->bool (value-of-expression condition env))))
      (if condition-val
          ; Execute body once and get both result and new environment
          (let* ((result-and-env (exec-statement-with-env body-scope env))
                 (body-result (car result-and-env))
                 (new-env (cdr result-and-env)))
            ; Check if body returned break or continue
            (cases expval body-result
              (break-val () new-env)  ; Break out of loop with current environment
              (continue-val () (exec-while-loop-for-env condition body-scope new-env))  ; Continue to next iteration
              (else 
                ; Continue looping with updated environment
                (exec-while-loop-for-env condition body-scope new-env))))
          env)))) ; Return environment when loop ends

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

; Helper function to execute statement and return both result and environment
(define exec-statement-with-env
  (lambda (stmt env)
    ; For statements that don't change environment, avoid double execution
    (cond
      [(and (pair? stmt) (eq? (car stmt) 'simple-stament))
       (let ((simple-stmt (cadr stmt)))
         (cond
           ; Print statements and other non-environment-changing expressions
           [(and (pair? simple-stmt) (eq? (car simple-stmt) 'expression))
            (let ((expr (cadr simple-stmt)))
              (if (is-assignment-expression? expr)
                  ; Assignment - execute once and return both result and new env
                  (let ((result (value-of-expression expr env))
                        (new-env (exec-assignment-for-env expr env)))
                    (cons result new-env))
                  ; Non-assignment - return result and same environment
                  (let ((result (value-of-expression expr env)))
                    (cons result env))))]
           ; Variable declarations
           [(and (pair? simple-stmt) (eq? (car simple-stmt) 'var-declaration))
            (let ((result (exec-var-declaration (cadr simple-stmt) env))
                  (new-env (exec-var-declaration-for-env (cadr simple-stmt) env)))
              (cons result new-env))]
           ; Break and continue
           [(and (pair? simple-stmt) (eq? (car simple-stmt) 'break-statement))
            (cons (break-val) env)]
           [(and (pair? simple-stmt) (eq? (car simple-stmt) 'continue-statement))
            (cons (continue-val) env)]
           [else 
            (let ((result (exec-statement stmt env))
                  (new-env (exec-statement-for-env stmt env)))
              (cons result new-env))]))]
      ; For other statement types, use the general approach
      [else 
       (let ((result (exec-statement stmt env))
             (new-env (exec-statement-for-env stmt env)))
         (cons result new-env))])))

; Helper function to execute statement list and return both result and environment
(define exec-statement-list-with-env
  (lambda (stmt-list env)
    (cond
      [(null? stmt-list) (cons (num-val 0) env)]
      [(null? (cdr stmt-list)) 
       ; Last statement - execute once and return both result and environment
       (exec-statement-with-env (car stmt-list) env)]
      [else 
       ; Execute current statement and check for break/continue
       (let* ((result-and-env (exec-statement-with-env (car stmt-list) env))
              (current-result (car result-and-env))
              (new-env (cdr result-and-env)))
         ; Check if current statement returned break or continue
         (cases expval current-result
           (break-val () result-and-env)  ; Propagate break up
           (continue-val () result-and-env)  ; Propagate continue up
           (else 
             ; Continue with next statement
             (exec-statement-list-with-env (cdr stmt-list) new-env))))])))

(provide (all-defined-out))