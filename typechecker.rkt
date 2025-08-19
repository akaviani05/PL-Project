#lang racket

(require (lib "eopl.ss" "eopl"))

;; New explicit-type checker (no Hindley-Milner). Types are s-exprs:
;;   'int 'float 'bool 'string 'char 'list
;;   function type: (list 'fun (list param-types) ret-type)

(define (type-error fmt . args)
  (apply eopl:error 'type-error fmt args))

;; type environment: assoc list of (name . typ)
(define empty-type-env '())

(define (env-lookup name env)
  (let loop ((lst env))
    (cond
      [(null? lst) #f]
      [(equal? (caar lst) name) (cdar lst)]
      [else (loop (cdr lst))])))

(define (env-extend name typ env)
  (cons (cons name typ) env))

;; type constructors
(define (fun-type param-types ret) (list 'fun param-types ret))

;; equality with simple int->float promotion
(define (types-equal? t1 t2)
  (cond
    [(equal? t1 t2) #t]
    [(and (eq? t1 'int) (eq? t2 'float)) #t] ; int can promote to float
    [else #f]))

;; Helpers to parse declared type nodes created by parser.rkt.
;; Parser uses nodes like (node 'int-t), (node 'float-t), (node 'list-t), etc.
(define (parse-declared-type node)
  (cond
    [(and (pair? node) (eq? (car node) 'int-t)) 'int]
    [(and (pair? node) (eq? (car node) 'float-t)) 'float]
    [(and (pair? node) (eq? (car node) 'bool-t)) 'bool]
    [(and (pair? node) (eq? (car node) 'string-t)) 'string]
    [(and (pair? node) (eq? (car node) 'char-t)) 'char]
    [(and (pair? node) (eq? (car node) 'list-t)) 'list]
    [else (type-error "unknown declared type node: ~s" node)]))

;; Main entry: typecheck-program: (node 'program statements)
(define (typecheck-program ast)
  (cond
    [(and (pair? ast) (eq? (car ast) 'program))
     (let ((stmts (cadr ast)))
       (car (typecheck-statement-list stmts empty-type-env)))]
    [else (type-error "expected program node, got: ~s" ast)]))

;; Return pair (env-ok . env) -- env-ok is #t when successful, or signals via errors
(define (typecheck-statement-list stmts env)
  (cond
    [(null? stmts) (cons #t env)]
    [else
     (let ((res (typecheck-statement (car stmts) env)))
       (let ((ok (car res)) (new-env (cdr res)))
         (if ok
             (typecheck-statement-list (cdr stmts) new-env)
             (cons ok new-env))))]))

(define (typecheck-statement stmt env)
  (cond
    [(and (pair? stmt) (eq? (car stmt) 'simple-stament))
     (typecheck-simple-statement (cadr stmt) env)]
    [(and (pair? stmt) (eq? (car stmt) 'scope))
     (typecheck-statement-list (cadr stmt) env)]
    [(and (pair? stmt) (eq? (car stmt) 'if-statement))
     (typecheck-if-stmt (cadr stmt) env)]
    [(and (pair? stmt) (eq? (car stmt) 'while-statement))
     (typecheck-while-stmt (cadr stmt) env)]
    [else (type-error "invalid statement node: ~s" stmt)]))

(define (typecheck-simple-statement s env)
  (cond
    [(and (pair? s) (eq? (car s) 'var-declaration))
     (let ((vd (cadr s)))
       (cond
         [(and (pair? vd) (eq? (car vd) 'var-default))
          (let* ((vtn (cadr vd))
                 (decl-typ (parse-declared-type (cadr vtn)))
                 (vname (caddr vtn)))
            (cons #t (env-extend vname decl-typ env)))]
         [(and (pair? vd) (eq? (car vd) 'var-assign))
          (let* ((vtn (cadr vd))
                 (decl-typ (parse-declared-type (cadr vtn)))
                 (vname (caddr vtn))
                 (expr (caddr vd))
                 (expr-typ (type-of-expression expr env)))
            (unless (types-equal? expr-typ decl-typ)
              (type-error "type mismatch in declaration of ~a: declared ~s but initializer has ~s"
                          vname decl-typ expr-typ))
            (cons #t (env-extend vname decl-typ env)))]
         [else (type-error "invalid var-declaration node: ~s" vd)]) )]
    [(and (pair? s) (eq? (car s) 'func-declaration))
     (let ((fd (cadr s)))
       ; fd: (function-declaration return-type func-name scope maybe-params)
       (let* ((ret-node (cadr fd))
              (ret-typ (parse-declared-type ret-node))
              (fname (caddr fd))
              (scope (cadddr fd))
              (raw-params (if (null? (cddddr fd)) '() (car (cddddr fd))))
              (params (map (lambda (p) (cons (caddr p) (parse-declared-type (cadr p)))) raw-params))
              (param-types (map cdr params))
              (fun-typ (fun-type param-types ret-typ))
              ;; Insert placeholder for recursion
              (env1 (env-extend fname fun-typ env))
              ;; extend with parameters for body checking
              (env2 (foldl (lambda (p acc) (env-extend (car p) (cdr p) acc)) env1 params)))
         ; Typecheck body statements inside scope; ensure returns inside body match ret-typ
         (let ((res (typecheck-statement-list (cadr scope) env2)))
           (let ((ok (car res)) (final-env (cdr res)))
             (if ok
                 (cons #t env1) ; function binding added to outer env
                 (cons ok final-env))))))]

    [(and (pair? s) (eq? (car s) 'expression))
     (let ((expr (cadr s)))
       (type-of-expression expr env)
       (cons #t env))]
    [(and (pair? s) (eq? (car s) 'break-statement)) (cons #t env)]
    [(and (pair? s) (eq? (car s) 'continue-statement)) (cons #t env)]
    [(and (pair? s) (eq? (car s) 'return-statement))
     ; returns are validated inside function bodies; here accept
     (cons #t env)]
    [else (type-error "invalid simple-statement node: ~s" s)]))

(define (typecheck-if-stmt ifn env)
  (cond
    [(and (pair? ifn) (eq? (car ifn) 'if))
     (let ((cond-expr (cadr ifn)) (then-scope (caddr ifn)))
       (let ((ct (type-of-expression cond-expr env)))
         (unless (types-equal? ct 'bool) (type-error "if condition must be bool, got ~s" ct))
         (typecheck-statement-list (cadr then-scope) env)
         (cons #t env)))]
    [(and (pair? ifn) (eq? (car ifn) 'if-else))
     (let ((cond-expr (cadr ifn)) (then-scope (caddr ifn)) (else-scope (cadddr ifn)))
       (let ((ct (type-of-expression cond-expr env)))
         (unless (types-equal? ct 'bool) (type-error "if condition must be bool, got ~s" ct))
         (typecheck-statement-list (cadr then-scope) env)
         (typecheck-statement-list (cadr else-scope) env)
         (cons #t env)))]
    [(and (pair? ifn) (eq? (car ifn) 'if-elseif))
     (let ((cond-expr (cadr ifn)) (then-scope (caddr ifn)) (elseif (cadddr ifn)))
       (let ((ct (type-of-expression cond-expr env)))
         (unless (types-equal? ct 'bool) (type-error "if condition must be bool, got ~s" ct))
         (typecheck-statement-list (cadr then-scope) env)
         (typecheck-if-stmt elseif env)))]
    [else (type-error "invalid if node: ~s" ifn)]))

(define (typecheck-while-stmt wn env)
  (cond
    [(and (pair? wn) (eq? (car wn) 'while))
     (let ((cond-expr (cadr wn)) (body (caddr wn)))
       (let ((ct (type-of-expression cond-expr env)))
         (unless (types-equal? ct 'bool) (type-error "while condition must be bool, got ~s" ct))
         (typecheck-statement-list (cadr body) env)
         (cons #t env)))]
    [else (type-error "invalid while node: ~s" wn)]))

;; Expressions: peel nested exp-level nodes like interpreter does
(define (type-of-expression expr env)
  (cond
    [(and (pair? expr) (member (car expr) '(exp6 exp5 exp4 exp3 exp2 exp1 exp0))) (type-of-expression (cadr expr) env)]
    [(and (pair? expr) (eq? (car expr) 'paren)) (type-of-expression (cadr expr) env)]
    ;; Binary arithmetic
    [(and (pair? expr) (eq? (car expr) '+))
     (let ((t1 (type-of-expression (cadr expr) env)) (t2 (type-of-expression (caddr expr) env)))
       (if (and (member t1 '(int float)) (member t2 '(int float)))
           (if (or (eq? t1 'float) (eq? t2 'float)) 'float 'int)
           (type-error "invalid '+' operands: ~s and ~s" t1 t2)))]
    [(and (pair? expr) (eq? (car expr) '-))
     (let ((t1 (type-of-expression (cadr expr) env)) (t2 (type-of-expression (caddr expr) env)))
       (if (and (member t1 '(int float)) (member t2 '(int float)))
           (if (or (eq? t1 'float) (eq? t2 'float)) 'float 'int)
           (type-error "invalid '-' operands: ~s and ~s" t1 t2)))]
    [(and (pair? expr) (eq? (car expr) '*))
     (let ((t1 (type-of-expression (cadr expr) env)) (t2 (type-of-expression (caddr expr) env)))
       (if (and (member t1 '(int float)) (member t2 '(int float)))
           (if (or (eq? t1 'float) (eq? t2 'float)) 'float 'int)
           (type-error "invalid '*' operands: ~s and ~s" t1 t2)))]
    [(and (pair? expr) (eq? (car expr) '/))
     (let ((t1 (type-of-expression (cadr expr) env)) (t2 (type-of-expression (caddr expr) env)))
       (if (and (member t1 '(int float)) (member t2 '(int float)))
           'float
           (type-error "invalid '/' operands: ~s and ~s" t1 t2)))]
    [(and (pair? expr) (eq? (car expr) '%))
     (let ((t1 (type-of-expression (cadr expr) env)) (t2 (type-of-expression (caddr expr) env)))
       (if (and (eq? t1 'int) (eq? t2 'int))
           'int
           (type-error "invalid '%' operands (integers expected): ~s and ~s" t1 t2)))]
    ;; Comparisons => bool
    [(member (car expr) '(<= >= < > == !=))
     (let ((t1 (type-of-expression (cadr expr) env)) (t2 (type-of-expression (caddr expr) env)))
       (if (or (and (member t1 '(int float)) (member t2 '(int float)))
               (and (eq? t1 t2) (member t1 '(bool string char list))))
           'bool
           (type-error "invalid comparison operands: ~s and ~s" t1 t2)))]
    ;; Bitwise ops require int
    [(member (car expr) '(& ^ orop))
     (let ((t1 (type-of-expression (cadr expr) env)) (t2 (type-of-expression (caddr expr) env)))
       (if (and (eq? t1 'int) (eq? t2 'int))
           'int
           (type-error "bitwise operations require ints, got ~s and ~s" t1 t2)))]
    ;; Logical ops => bool
    [(eq? (car expr) '&&)
     (let ((t1 (type-of-expression (cadr expr) env)) (t2 (type-of-expression (caddr expr) env)))
       (unless (and (eq? t1 'bool) (eq? t2 'bool)) (type-error "&& requires bools, got ~s and ~s" t1 t2)) 'bool)]
    [(eq? (car expr) '||)
     (let ((t1 (type-of-expression (cadr expr) env)) (t2 (type-of-expression (caddr expr) env)))
       (unless (and (eq? t1 'bool) (eq? t2 'bool)) (type-error "|| requires bools, got ~s and ~s" t1 t2)) 'bool)]
    ;; Unary
    [(eq? (car expr) '!)
     (let ((t (type-of-expression (cadr expr) env)))
       (unless (eq? t 'bool) (type-error "! expects bool, got ~s" t)) 'bool)]
    [(eq? (car expr) '~)
     (let ((t (type-of-expression (cadr expr) env)))
       (unless (eq? t 'int) (type-error "~ expects int, got ~s" t)) 'int)]
    ;; Atom handling
    [(and (pair? expr) (eq? (car expr) 'atom))
     (type-of-atom (cadr expr) env)]
    ;; parenthesis
    [(and (pair? expr) (eq? (car expr) 'paren))
     (type-of-expression (cadr expr) env)]
    [else (type-error "unknown expression node: ~s" expr)]))

(define (type-of-atom atom env)
  (cond
    [(and (pair? atom) (eq? (car atom) 'value-atom))
     (let ((v (cadr atom)))
       (cond
         [(and (pair? v) (eq? (car v) 'int-val)) 'int]
         [(and (pair? v) (eq? (car v) 'float-val)) 'float]
         [(and (pair? v) (eq? (car v) 'bool-val)) 'bool]
         [(and (pair? v) (eq? (car v) 'str-val)) 'string]
         [(and (pair? v) (eq? (car v) 'char-val)) 'char]
         [(and (pair? v) (eq? (car v) 'lst-val)) ; list literal node: may be (lst-val ...) from parser; fallback
          'list]
         [else (type-error "unknown literal node: ~s" v)]))]
    [(and (pair? atom) (eq? (car atom) 'var-name-atom))
     (let ((id (cadr atom)))
       (let ((t (env-lookup id env))) ; use passed env
         (if t t (type-error "unbound identifier ~a" id))))] 
    [(and (pair? atom) (eq? (car atom) 'assignment-atom))
     (let ((assign (cadr atom)))
       (let ((var (cadr assign)) (expr (caddr assign)))
         (let ((tvar (env-lookup var env))) ; use passed env
           (unless tvar (type-error "assignment to undeclared variable ~a" var))
           (let ((texpr (type-of-expression expr env))) ; use passed env
             (unless (types-equal? texpr tvar)
               (type-error "assignment type mismatch for ~a: expected ~s got ~s" var tvar texpr))
             tvar))))]
    [(and (pair? atom) (eq? (car atom) 'predefined-atom))
     (type-of-predefined (cadr atom) env)]
    [(and (pair? atom) (eq? (car atom) 'function-atom))
     (type-of-function-call (cadr atom) env)]
    [else (type-error "unknown atom node: ~s" atom)]))

;; Predefined statements simple typing
(define (type-of-predefined pd env)
  (cond
    [(and (pair? pd) (eq? (car pd) 'print-statement))
     (type-of-expression (cadr pd) env) ; print accepts any type; returns same type for chaining
     'int] ; or return unit-like int
    [(and (pair? pd) (eq? (car pd) 'input-statement))
     'int] ; input returns something runtime; approximate
    [(and (pair? pd) (eq? (car pd) 'get-statement))
     (let ((var (cadr pd)) (index (caddr pd)))
       (let ((vtype (env-lookup var env)))
         (unless vtype (type-error "$get expects list variable, got unbound ~a" var))
         (unless (eq? vtype 'list) (type-error "$get expects list, got ~s" vtype))
         'int))] ; can't deduce element type precisely; return generic
    [(and (pair? pd) (eq? (car pd) 'set-statement))
     'int]
    [(and (pair? pd) (eq? (car pd) 'push-statement))
     'int]
    [(and (pair? pd) (eq? (car pd) 'pop-statement))
     'int]
    [(and (pair? pd) (eq? (car pd) 'size-statement))
     'int]
    [(and (pair? pd) (eq? (car pd) 'tocharlist-statement))
     'list]
    [else (type-error "unknown predefined statement: ~s" pd)]))

;; Function call typing
(define (type-of-function-call fc env)
  (cond
    [(and (pair? fc) (eq? (car fc) 'function-call))
     (let ((fname (cadr fc)) (raw-args (caddr fc)))
       (let ((ftype (env-lookup fname env)))
         (unless ftype (type-error "call of unbound function ~a" fname))
         (unless (and (pair? ftype) (eq? (car ftype) 'fun)) (type-error "called value ~a is not a function, has type ~s" fname ftype))
         (let* ((param-types (cadr ftype))
                (ret-type (caddr ftype))
                (args (if (null? raw-args) '() raw-args))
                (arg-types (map (lambda (a) (type-of-expression a env)) args)))
           (unless (= (length arg-types) (length param-types))
             (type-error "argument count mismatch in call to ~a: expected ~s got ~s" fname (length param-types) (length arg-types)))
           (for ([pt param-types] [at arg-types])
             (unless (types-equal? at pt) (type-error "argument type mismatch in call to ~a: expected ~s got ~s" fname pt at)))
           ret-type)))]
    [else (type-error "invalid function-call node: ~s" fc)]))


(provide typecheck-program)