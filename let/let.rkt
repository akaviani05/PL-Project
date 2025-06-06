#lang racket

(require "datatypes.rkt")
(require "environment.rkt")
(require (lib "eopl.ss" "eopl"))

(define (report-invalid-expression!) (eopl:error 'invalid-expression "this expression is invalid in the current language!"))

(define value-of-program
 (lambda (pgm) (cases program pgm
    (a-program (expr) (value-of expr (init-env))))))

(define value-of
 (lambda (exp env) (cases expression exp
    (const-exp (num) (num-val num))
    (var-exp (var) (apply-env var env))
    (diff-exp (exp1 exp2)
        (num-val (- (expval->num (value-of exp1 env)) (expval->num (value-of exp2 env)))))
    (zero?-exp (expr)
        (bool-val (zero? (expval->num (value-of expr env)))))
    (if-exp (exp1 exp2 exp3)
        (if (expval->bool (value-of exp1 env)) (value-of exp2 env) (value-of exp3 env)))
    (let-exp (var expr body)
        (value-of body (extend-env var (value-of expr env) env)))
    (else (report-invalid-expression!)))))
