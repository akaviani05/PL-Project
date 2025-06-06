#lang racket

(require (lib "eopl.ss" "eopl"))

(define (report-expval-extractor-error! type) (eopl:error 'invalid-value "invalid value cast to ~s" type))

(define-datatype environment environment?
  (empty-environment)
  (extend-environment (var string?) (val expval?) (env environment?)))

(define expval->num
 (lambda (val) (cases expval val
    (num-val (num) num)
    (else (report-expval-extractor-error! "number")))))

(define expval->bool
 (lambda (val) (cases expval val
    (bool-val (bool) bool)
    (else (report-expval-extractor-error! "boolean")))))

(define expval->proc
 (lambda (val) (cases expval val
    (proc-val (proc) (proc))
    (else (report-expval-extractor-error! "proc")))))

(define expval->ref
 (lambda (val) (cases expval val
    (ref-val (int) int)
    (else (report-expval-extractor-error! "reference")))))

(define-datatype proc proc?
 (procedure (var string?) (body expression?) (env environment?)))

(define-datatype program program?
 (a-program (expr expression?)))

(define-datatype expression expression?
 (const-exp (num number?))
 (diff-exp (exp1 expression?) (exp2 expression?))
 (zero?-exp (expr expression?))
 (if-exp (exp1 expression?) (exp2 expression?) (exp3 expression?))
 (var-exp (var string?))
 (let-exp (var string?) (expr expression?) (body expression?))
 (proc-exp (var string?) (body expression?))
 (call-exp (rator expression?) (rand expression?))
 (newref-exp (expr expression?))
 (deref-exp (expr expression?))
 (setref-exp (exp1 expression?) (exp2 expression?)))

(define-datatype expval expval?
 (num-val (num number?))
 (bool-val (bool boolean?))
 (proc-val (proc proc?))
 (ref-val (int integer?)))

(provide (all-defined-out))
