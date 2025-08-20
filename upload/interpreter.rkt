#lang racket

(require "datatypes.rkt")
(require "environment.rkt")
(require (lib "eopl.ss" "eopl"))


(provide value-of-program)


(define (report-invalid-expression! expr) 
  (eopl:error 'invalid-expression "this expression is invalid: ~s" expr))

(define (report-invalid-statement! stmt) 
  (eopl:error 'invalid-statement "this statement is invalid: ~s" stmt))


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


(define equal-values?
  (lambda (val1 val2)
    (cond
      
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
            (function-val (p2 b2 e2) #t) 
            (else #f)))
         (list-val (lst1)
          (cases expval val2
            (list-val (lst2) (equal-lists? lst1 lst2))
            (else #f))))]
      [else #f])))


(define equal-lists?
  (lambda (lst1 lst2)
    (cond
      [(and (null? lst1) (null? lst2)) #t]
      [(or (null? lst1) (null? lst2)) #f]
      [else 
       (and (equal-values? (car lst1) (car lst2))
            (equal-lists? (cdr lst1) (cdr lst2)))])))


(define extract-parameter-names
  (lambda (param-list)
    (cond
      [(null? param-list) '()]
      [(pair? param-list)
       
       (map (lambda (param)
              (cond
                [(and (pair? param) (eq? (car param) 'var-type-name))
                 
                 (caddr param)]
                [else param]))
            param-list)]
      [else '()])))


(define extract-argument-expressions
  (lambda (arg-list)
    (cond
      [(null? arg-list) '()]
      [(pair? arg-list) arg-list]  
      [else (list arg-list)])))


(define value-of-program
  (lambda (ast)
    (cond
      [(and (pair? ast) (eq? (car ast) 'program))
       (let ((statements (cadr ast)))
         (let ((result-env-pair (exec-statement-list-tof statements (init-env))))
           (car result-env-pair)))]
      [(and (pair? ast) (eq? (car ast) 'expression))  
       (value-of-expression (cadr ast) (init-env))]
      [else (report-invalid-expression! ast)])))





(define exec-statement-list-tof
  (lambda (stmt-list env)
    (cond
      [(null? stmt-list) (cons (num-val 0) env)]
      [(null? (cdr stmt-list)) 
       
       (exec-statement-tof (car stmt-list) env)]
      [else 
       
       (let* ((result-env-pair (exec-statement-tof (car stmt-list) env))
              (current-result (car result-env-pair))
              (new-env (cdr result-env-pair)))
         
         (cases expval current-result
           (break-val () result-env-pair)  
           (continue-val () result-env-pair)  
           (else 
             
             (exec-statement-list-tof (cdr stmt-list) new-env))))])))


(define exec-statement-tof
  (lambda (stmt env)
    (cond
      [(and (pair? stmt) (eq? (car stmt) 'simple-stament))
       (exec-simple-statement-tof (cadr stmt) env)]
      [(and (pair? stmt) (eq? (car stmt) 'scope))
       
       (let ((statements (cadr stmt)))
         (exec-statement-list-tof statements env))]
      [(and (pair? stmt) (eq? (car stmt) 'if-statement))
       (exec-if-statement-tof (cadr stmt) env)]
      [(and (pair? stmt) (eq? (car stmt) 'while-statement))
       (exec-while-statement-tof (cadr stmt) env)]
      [else (cons (report-invalid-statement! stmt) env)])))


(define exec-simple-statement-tof
  (lambda (stmt env)
    (cond
      [(and (pair? stmt) (eq? (car stmt) 'var-declaration))
       (exec-var-declaration-tof (cadr stmt) env)]
      [(and (pair? stmt) (eq? (car stmt) 'func-declaration))
       (exec-function-declaration-tof (cadr stmt) env)]
      [(and (pair? stmt) (eq? (car stmt) 'expression))
       (let ((expr (cadr stmt)))
         (if (is-assignment-expression? expr)
             
             (let ((result (value-of-expression expr env))
                   (new-env (exec-assignment-for-env expr env)))
               (cons result new-env))
             
             (if (is-predefined-expression? expr)
                 
                 (value-of-predefined-statement-tof-from-expr expr env)
                 
                 (let ((result (value-of-expression expr env)))
                   (cons result env)))))]
      [(and (pair? stmt) (eq? (car stmt) 'break-statement))
       (cons (break-val) env)]
      [(and (pair? stmt) (eq? (car stmt) 'continue-statement))
       (cons (continue-val) env)]
      [(and (pair? stmt) (eq? (car stmt) 'return-statement))
       (exec-return-statement-tof (cadr stmt) env)]
      [else (cons (report-invalid-statement! stmt) env)])))


(define is-predefined-expression?
  (lambda (expr)
    (cond
      [(and (pair? expr) (member (car expr) '(exp6 exp5 exp4 exp3 exp2 exp1 exp0)))
       (is-predefined-expression? (cadr expr))]
      [(and (pair? expr) (eq? (car expr) 'atom))
       (let ((atom (cadr expr)))
         (and (pair? atom) (eq? (car atom) 'predefined-atom)))]
      [else #f])))


(define value-of-predefined-statement-tof-from-expr
  (lambda (expr env)
    (cond
      [(and (pair? expr) (member (car expr) '(exp6 exp5 exp4 exp3 exp2 exp1 exp0)))
       (value-of-predefined-statement-tof-from-expr (cadr expr) env)]
      [(and (pair? expr) (eq? (car expr) 'atom))
       (let ((atom (cadr expr)))
         (if (and (pair? atom) (eq? (car atom) 'predefined-atom))
             (value-of-predefined-statement-tof (cadr atom) env)
             (cons (value-of-expression expr env) env)))]
      [else (cons (value-of-expression expr env) env)])))


(define exec-var-declaration-tof
  (lambda (var-decl env)
    (cond
      [(and (pair? var-decl) (eq? (car var-decl) 'var-assign))
       
       (let* ((var-type-name (cadr var-decl))
              (init-expr (caddr var-decl))
              (var-name (extract-var-name var-type-name))
              (val (value-of-expression init-expr env))
              (new-env (extend-env var-name val env)))
         (cons val new-env))]
      [(and (pair? var-decl) (eq? (car var-decl) 'var-default))
       
       (let* ((var-type-name (cadr var-decl))
              (var-name (extract-var-name var-type-name))
              (default-val (get-default-value var-type-name))
              (new-env (extend-env var-name default-val env)))
         (cons default-val new-env))]
      [else (cons (report-invalid-statement! var-decl) env)])))


(define exec-function-declaration-tof
  (lambda (func-decl env)
    (cond
      [(and (pair? func-decl) (eq? (car func-decl) 'function-declaration))
       
       (let* ((return-type (cadr func-decl))
              (func-name (caddr func-decl))
              (scope (cadddr func-decl))
              (raw-params (if (null? (cddddr func-decl)) '() (car (cddddr func-decl))))
              (params (extract-parameter-names raw-params)))
         
         
         (let* ((extended-env (extend-env func-name (num-val 0) env))  
                (func-val (function-val params scope extended-env)))
           
           (let ((final-env (extend-env func-name func-val env)))
             (cons func-val final-env))))]
      [else (cons (report-invalid-statement! func-decl) env)])))


(define exec-if-statement-tof
  (lambda (if-stmt env)
    (cond
      
      [(and (pair? if-stmt) (eq? (car if-stmt) 'if))
       (let* ((condition (cadr if-stmt))
              (then-scope (caddr if-stmt))
              (condition-val (expval->bool (value-of-expression condition env))))
         (if condition-val
             (exec-statement-tof then-scope env)
             (cons (num-val 0) env)))] 
      
      
      [(and (pair? if-stmt) (eq? (car if-stmt) 'if-else))
       (let* ((condition (cadr if-stmt))
              (then-scope (caddr if-stmt))
              (else-scope (cadddr if-stmt))
              (condition-val (expval->bool (value-of-expression condition env))))
         (if condition-val
             (exec-statement-tof then-scope env)
             (exec-statement-tof else-scope env)))]
      
      
      [(and (pair? if-stmt) (eq? (car if-stmt) 'if-elseif))
       (let* ((condition (cadr if-stmt))
              (then-scope (caddr if-stmt))
              (elseif-stmt (cadddr if-stmt))
              (condition-val (expval->bool (value-of-expression condition env))))
         (if condition-val
             (exec-statement-tof then-scope env)
             (exec-if-statement-tof elseif-stmt env)))]
      
      [else (cons (report-invalid-statement! if-stmt) env)])))


(define exec-while-statement-tof
  (lambda (while-stmt env)
    (cond
      
      [(and (pair? while-stmt) (eq? (car while-stmt) 'while))
       (let* ((condition (cadr while-stmt))
              (body-scope (caddr while-stmt)))
         (exec-while-loop-tof condition body-scope env))]
      
      [else (cons (report-invalid-statement! while-stmt) env)])))


(define exec-while-loop-tof
  (lambda (condition body-scope env)
    (let ((condition-val (expval->bool (value-of-expression condition env))))
      (if condition-val
          
          (let* ((result-env-pair (exec-statement-tof body-scope env))
                 (body-result (car result-env-pair))
                 (new-env (cdr result-env-pair)))
            
            (cases expval body-result
              (break-val () (cons (num-val 0) new-env))  
              (continue-val () (exec-while-loop-tof condition body-scope new-env))  
              (else 
                
                (exec-while-loop-tof condition body-scope new-env))))
          (cons (num-val 0) env))))) 





(define exec-statement-list
  (lambda (stmt-list env)
    (let ((result-env-pair (exec-statement-list-tof stmt-list env)))
      (car result-env-pair))))


(define exec-statement-list-for-env
  (lambda (stmt-list env)
    (let ((result-env-pair (exec-statement-list-tof stmt-list env)))
      (cdr result-env-pair))))


(define exec-statement
  (lambda (stmt env)
    (let ((result-env-pair (exec-statement-tof stmt env)))
      (car result-env-pair))))


(define exec-statement-for-env
  (lambda (stmt env)
    (let ((result-env-pair (exec-statement-tof stmt env)))
      (cdr result-env-pair))))


(define exec-simple-statement
  (lambda (stmt env)
    (let ((result-env-pair (exec-simple-statement-tof stmt env)))
      (car result-env-pair))))


(define exec-simple-statement-for-env
  (lambda (stmt env)
    (let ((result-env-pair (exec-simple-statement-tof stmt env)))
      (cdr result-env-pair))))


(define exec-var-declaration
  (lambda (var-decl env)
    (let ((result-env-pair (exec-var-declaration-tof var-decl env)))
      (car result-env-pair))))


(define exec-var-declaration-for-env
  (lambda (var-decl env)
    (let ((result-env-pair (exec-var-declaration-tof var-decl env)))
      (cdr result-env-pair))))




(define extract-var-name
  (lambda (var-type-name)
    (cond
      [(and (pair? var-type-name) (eq? (car var-type-name) 'var-type-name))
       (caddr var-type-name)] 
      [else (report-invalid-expression! var-type-name)])))


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


(define is-assignment-expression?
  (lambda (expr)
    (cond
      
      [(and (pair? expr) (member (car expr) '(exp6 exp5 exp4 exp3 exp2 exp1 exp0)))
       (is-assignment-expression? (cadr expr))]
      
      
      [(and (pair? expr) (eq? (car expr) 'atom))
       (let ((atom (cadr expr)))
         (and (pair? atom) (eq? (car atom) 'assignment-atom)))]
      
      [else #f])))


(define exec-assignment-for-env
  (lambda (expr env)
    (cond
      
      [(and (pair? expr) (member (car expr) '(exp6 exp5 exp4 exp3 exp2 exp1 exp0)))
       (exec-assignment-for-env (cadr expr) env)]
      
      
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


(define value-of-expression
  (lambda (expr env)
    (cond
      
      [(and (pair? expr) (eq? (car expr) 'exp6)) (value-of-expression (cadr expr) env)]
      [(and (pair? expr) (eq? (car expr) 'exp5)) (value-of-expression (cadr expr) env)]
      [(and (pair? expr) (eq? (car expr) 'exp4)) (value-of-expression (cadr expr) env)]
      [(and (pair? expr) (eq? (car expr) 'exp3)) (value-of-expression (cadr expr) env)]
      [(and (pair? expr) (eq? (car expr) 'exp2)) (value-of-expression (cadr expr) env)]
      [(and (pair? expr) (eq? (car expr) 'exp1)) (value-of-expression (cadr expr) env)]
      [(and (pair? expr) (eq? (car expr) 'exp0)) (value-of-expression (cadr expr) env)]
      
      
      [(and (pair? expr) (eq? (car expr) '+))
       (let ((expval1 (value-of-expression (cadr expr) env))
             (expval2 (value-of-expression (caddr expr) env)))

         (cond
           [(and (cases expval expval1 (string-val (str) #t) (else #f))
                 (cases expval expval2 (char-val (ch) #t) (else #f)))
            (cases expval expval1
              (string-val (str1)
                (cases expval expval2
                  (char-val (ch2)
                    (string-val (string-append str1 ch2)))
                  (else (eopl:error 'type-error "Expected char for string concatenation, got ~s" expval2))))
              (else (eopl:error 'type-error "Expected string for concatenation, got ~s" expval1)))]
           [else
            (let ((val1 (extract-numeric-value expval1))
                  (val2 (extract-numeric-value expval2)))
              (create-numeric-result (+ val1 val2) expval1 expval2))]))]
      
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
               
               (if (and (not (is-float-expval? expval1)) (not (is-float-expval? expval2)))
                   
                   (num-val (quotient (inexact->exact val1) (inexact->exact val2)))
                   
                   (create-numeric-result (/ val1 val2) expval1 expval2)))))]

      [(and (pair? expr) (eq? (car expr) '%))
       (let ((expval1 (value-of-expression (cadr expr) env))
             (expval2 (value-of-expression (caddr expr) env)))
         (let ((val1 (extract-numeric-value expval1))
               (val2 (extract-numeric-value expval2)))
           (if (= val2 0)
               (eopl:error 'division-by-zero "modulo by zero")
               (num-val (remainder (inexact->exact (floor val1)) (inexact->exact (floor val2)))))))]
      
      
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
      
      
      [(and (pair? expr) (eq? (car expr) 'atom))
       (value-of-atom (cadr expr) env)]
      
      
      [(and (pair? expr) (eq? (car expr) 'paren))
       (value-of-expression (cadr expr) env)]
      
      [else (report-invalid-expression! expr)])))


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


(define value-of-predefined-statement-tof
  (lambda (predefined-stmt env)
    (cond
      [(and (pair? predefined-stmt) (eq? (car predefined-stmt) 'print-statement))
       
       (let ((val (value-of-expression (cadr predefined-stmt) env)))
         (display (expval->string-for-print val))
         (newline)
         (cons val env))] 
      [(and (pair? predefined-stmt) (eq? (car predefined-stmt) 'input-statement))
       (value-of-input-statement-tof (cadr predefined-stmt) env)]
      [(and (pair? predefined-stmt) (eq? (car predefined-stmt) 'get-statement))
       (let ((val (value-of-get-statement (cadr predefined-stmt) (caddr predefined-stmt) env)))
         (cons val env))]
      [(and (pair? predefined-stmt) (eq? (car predefined-stmt) 'set-statement))
       (value-of-set-statement-tof (cadr predefined-stmt) (caddr predefined-stmt) (cadddr predefined-stmt) env)]
      [(and (pair? predefined-stmt) (eq? (car predefined-stmt) 'push-statement))
       (value-of-push-statement-tof (cadr predefined-stmt) (caddr predefined-stmt) env)]
      [(and (pair? predefined-stmt) (eq? (car predefined-stmt) 'pop-statement))
       (value-of-pop-statement-tof (cadr predefined-stmt) env)]
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


(define value-of-predefined-statement
  (lambda (predefined-stmt env)
    (let ((result-env-pair (value-of-predefined-statement-tof predefined-stmt env)))
      (car result-env-pair))))


(define value-of-input-statement
  (lambda (var-name env)
    (display "Input: ")
    (let ((input-str (read-line)))
      
      (let ((val (cond
                   [(string->number input-str) (num-val (string->number input-str))]
                   [(string=? input-str "true") (bool-val #t)]
                   [(string=? input-str "false") (bool-val #f)]
                   [else (string-val input-str)])))
        val))))


(define value-of-input-statement-tof
  (lambda (var-name env)
    (display "Input: ")
    (let ((input-str (read-line)))
      
      (let ((val (cond
                   [(string->number input-str) (num-val (string->number input-str))]
                   [(string=? input-str "true") (bool-val #t)]
                   [(string=? input-str "false") (bool-val #f)]
                   [else (string-val input-str)])))
        
        (let ((new-env (extend-env var-name val env)))
          (cons val new-env))))))


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


(define value-of-set-statement
  (lambda (var-name index-expr value-expr env)
    (let ((result-env-pair (value-of-set-statement-tof var-name index-expr value-expr env)))
      (car result-env-pair))))


(define value-of-push-statement
  (lambda (var-name value-expr env)
    (let ((result-env-pair (value-of-push-statement-tof var-name value-expr env)))
      (car result-env-pair))))


(define value-of-pop-statement
  (lambda (var-name env)
    (let ((result-env-pair (value-of-pop-statement-tof var-name env)))
      (car result-env-pair))))


(define value-of-set-statement-tof
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


(define value-of-push-statement-tof
  (lambda (var-name value-expr env)
    (let ((list-expval (apply-env var-name env))
          (new-val (value-of-expression value-expr env)))
      (cases expval list-expval
        (list-val (lst)
         (let* ((new-lst (append lst (list new-val)))
                (new-list-val (list-val new-lst)))
           (cons new-list-val (extend-env var-name new-list-val env))))
        (else (eopl:error 'type-error "expected list, got ~s" list-expval))))))


(define value-of-pop-statement-tof
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


(define list-set
  (lambda (lst index new-val)
    (cond
      [(= index 0) (cons new-val (cdr lst))]
      [else (cons (car lst) (list-set (cdr lst) (- index 1) new-val))])))


(define expval->string-for-print
  (lambda (val)
    (cases expval val
      (num-val (num) (number->string num))
      (float-val (fl) (number->string fl))
      (bool-val (bool) (if bool "true" "false"))
      (string-val (str) 
        
        (let ((clean-str (if (and (> (string-length str) 1)
                                 (eq? (string-ref str 0) #\")
                                 (eq? (string-ref str (- (string-length str) 1)) #\"))
                            (substring str 1 (- (string-length str) 1))
                            str)))
          clean-str))
      (char-val (ch) 
        
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


(define value-of-assignment
  (lambda (assignment env)
    (cond
      [(and (pair? assignment) (eq? (car assignment) 'assignment))
       (let* ((var-name (cadr assignment))
              (expr (caddr assignment))
              (val (value-of-expression expr env)))
         val)] 
      [else (report-invalid-expression! assignment)])))




(define exec-return-statement-tof
  (lambda (return-stmt env)
    (cond
      
      [(null? return-stmt)
       (cons (return-val (num-val 0)) env)]
      
      [(and (pair? return-stmt) (eq? (car return-stmt) 'return-statement))
       (exec-return-statement-tof (cdr return-stmt) env)]
      
      [(pair? return-stmt)
       (let ((expr (car return-stmt)))
         (let ((val (value-of-expression expr env)))
           (cons (return-val val) env)))]
      [else (cons (report-invalid-statement! return-stmt) env)])))


(define value-of-function-call
  (lambda (func-call env)
    (cond
      [(and (pair? func-call) (eq? (car func-call) 'function-call))
       
       (let* ((func-name (cadr func-call))
              (raw-args (caddr func-call))
              (args (extract-argument-expressions raw-args))
              (func-val (apply-env func-name env)))
         (cases expval func-val
           (function-val (params body closure-env)
             
             (let ((arg-vals (map (lambda (arg) (value-of-expression arg env)) args)))
               
               
               (let* ((func-env (extend-env func-name func-val closure-env))
                      (param-env (bind-parameters params arg-vals func-env)))
                 
                 (let ((result-env-pair (exec-statement-tof body param-env)))
                   (let ((result (car result-env-pair)))
                     
                     (cases expval result
                       (return-val (val) val)  
                       (else result)))))))     
           (else (report-invalid-expression! (list "not a function:" func-name)))))]
      [else (report-invalid-expression! func-call)])))




(define value-of-list-literal
  (lambda (value-seq)
    (cond
      [(null? value-seq) (list-val '())]
      [(pair? value-seq)
       (let ((values (map value-of-value value-seq)))
         (list-val values))]
      [else (list-val (list (value-of-value value-seq)))])))


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
       
       (let* ((param (car params))
              (param-name (if (and (pair? param) (eq? (car param) 'var-type-name))
                             (caddr param)  
                             param))        
              (arg-val (car args))
              (new-env (extend-env param-name arg-val env)))
         (bind-parameters (cdr params) (cdr args) new-env))])))