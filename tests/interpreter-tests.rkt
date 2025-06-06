#lang racket

(require "../datatypes.rkt")
(require "../environment.rkt")
(require "../interpreter.rkt")
(require (lib "eopl.ss" "eopl"))

; Helper function to create simple test programs
(define make-test-program
  (lambda (expr)
    (a-program (extend-stmt-seq (expression-stmt expr) (empty-stmt-seq)))))

; Test functions for basic arithmetic
(define test-add
  (lambda (n1 n2)
    (value-of-program 
      (make-test-program (add-exp (const-exp n1) (const-exp n2))))))

(define test-sub
  (lambda (n1 n2)
    (value-of-program 
      (make-test-program (sub-exp (const-exp n1) (const-exp n2))))))

; Test function for complex expressions
(define test-complex
  (lambda ()
    ; Test: 5 + (10 - 3) = 12
    (let ((complex-expr (add-exp (const-exp 5) (sub-exp (const-exp 10) (const-exp 3)))))
      (value-of complex-expr (init-env)))))

; Test function with variables
(define test-variables
  (lambda ()
    ; Test variable declaration and usage
    (let* ((env1 (extend-env "x" (num-val 5) (init-env)))
           (env2 (extend-env "y" (num-val 3) env1))
           (expr (add-exp (var-exp "x") (var-exp "y"))))
      (value-of expr env2))))

; Test function for variable declarations
(define test-var-declaration
  (lambda ()
    ; Test: int x = 10; x + 5
    (let* ((var-decl-stmt (var-dec-stmt (int-type) "x" (const-exp 10)))
           (add-stmt (expression-stmt (add-exp (var-exp "x") (const-exp 5))))
           (program (a-program (extend-stmt-seq var-decl-stmt 
                                              (extend-stmt-seq add-stmt (empty-stmt-seq))))))
      (value-of-program program))))

; Test function for assignments
(define test-assignment
  (lambda ()
    ; Test: int x = 5; x = x + 3; x
    (let* ((var-decl-stmt (var-dec-stmt (int-type) "x" (const-exp 5)))
           (assign-stmt (assignment-stmt "x" (add-exp (var-exp "x") (const-exp 3))))
           (expr-stmt (expression-stmt (var-exp "x")))
           (program (a-program (extend-stmt-seq var-decl-stmt 
                                              (extend-stmt-seq assign-stmt 
                                                              (extend-stmt-seq expr-stmt (empty-stmt-seq)))))))
      (value-of-program program))))

; Test runner function
(define run-all-tests
  (lambda ()
    (displayln "Running interpreter tests...")
    (displayln "")
    
    (displayln "Test 1: Basic addition (5 + 3)")
    (displayln (test-add 5 3))
    
    (displayln "Test 2: Basic subtraction (10 - 3)")
    (displayln (test-sub 10 3))
    
    (displayln "Test 3: Complex expression (5 + (10 - 3))")
    (displayln (test-complex))
    
    (displayln "Test 4: Variables (x=5, y=3, x+y)")
    (displayln (test-variables))
    
    (displayln "Test 5: Negative result (3 - 10)")
    (displayln (test-sub 3 10))
    
    (displayln "Test 6: Variable declaration (int x = 10; x + 5)")
    (displayln (test-var-declaration))
    
    (displayln "Test 7: Assignment (int x = 5; x = x + 3; x)")
    (displayln (test-assignment))
    
    (displayln "")
    (displayln "All tests completed!")))

(provide (all-defined-out))
