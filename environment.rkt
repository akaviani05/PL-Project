#lang racket

(require "datatypes.rkt")
(require (lib "eopl.ss" "eopl"))

; Error reporting for unbound variables
(define (report-no-binding-found! var) 
  (eopl:error 'binding-dismatch "\n\tidentifier ~s is used before its declaration!" var))

; Environment operations
(define extend-env 
  (lambda (var val env) 
    (extend-environment var val env)))

(define init-env 
  (lambda () 
    (empty-environment)))

(define apply-env 
  (lambda (var env) 
    (cases environment env
      (empty-environment () (report-no-binding-found! var))
      (extend-environment (saved-var val saved-env) 
        (if (equal? var saved-var) 
            val 
            (apply-env var saved-env))))))

; Check if variable exists in environment
(define in-env?
  (lambda (var env)
    (cases environment env
      (empty-environment () #f)
      (extend-environment (saved-var val saved-env)
        (if (equal? var saved-var)
            #t
            (in-env? var saved-env))))))

(provide (all-defined-out))