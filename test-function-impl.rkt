#lang racket

(require "datatypes.rkt")
(require "environment.rkt")
(require "interpreter.rkt")
(require (lib "eopl.ss" "eopl"))

; Test function value creation
(define test-function-value
  (lambda ()
    (display "Testing function value creation...\n")
    (let* ((params '())
           (body '(simple-statement (return-statement (expression (atom (value-atom (int-val 42)))))))
           (env (init-env))
           (func-val (function-val params body env)))
      (cases expval func-val
        (function-val (p b e) 
          (display "Function value created successfully\n")
          #t)
        (else 
          (display "Failed to create function value\n")
          #f)))))

; Test return value creation
(define test-return-value
  (lambda ()
    (display "Testing return value creation...\n")
    (let* ((val (num-val 42))
           (ret-val (return-val val)))
      (cases expval ret-val
        (return-val (v) 
          (display "Return value created successfully\n")
          #t)
        (else 
          (display "Failed to create return value\n")
          #f)))))

; Run tests
(display "Starting function implementation tests...\n")
(test-function-value)
(test-return-value)
(display "Function implementation tests completed!\n")
