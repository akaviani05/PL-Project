#lang racket

(require "interpreter.rkt")
(require "lexer.rkt")
(require "parser.rkt")

; Helper function to parse and interpret code (following test-file.rkt pattern)
(define test-parse-and-interpret
  (lambda (source-code)
    (displayln (format "Testing: ~s" source-code))
    (let* ((input-port (open-input-string source-code))
           (ast (full-parser (lambda () (full-lexer input-port)))))
      (let ((result (value-of-program ast)))
        (displayln (format "Result: ~s" result))
        result))))

; Test simple function declaration
(define test-simple-function
  (lambda ()
    (displayln "=== Testing Simple Function Declaration ===")
    (test-parse-and-interpret "int getValue() { return 42; };")))

; Test function declaration with no return value
(define test-void-function
  (lambda ()
    (displayln "=== Testing Void Function Declaration ===")
    (test-parse-and-interpret "int doSomething() { };")))

; Test function declaration with parameters
(define test-function-with-params
  (lambda ()
    (displayln "=== Testing Function Declaration with Parameters ===")
    (test-parse-and-interpret "int add(int a, int b) { return a + b; };")))

; Test function call (simple case)
(define test-function-call
  (lambda ()
    (displayln "=== Testing Function Call ===")
    (test-parse-and-interpret "int getValue() { return 42; }; getValue();")))

; Test function call with assignment
(define test-function-call-assignment
  (lambda ()
    (displayln "=== Testing Function Call with Assignment ===")
    (test-parse-and-interpret "int getValue() { return 42; }; int result = getValue();")))

; Test function call with arguments
(define test-function-call-with-args
  (lambda ()
    (displayln "=== Testing Function Call with Arguments ===")
    (test-parse-and-interpret "int add(int a, int b) { return a + b; }; add(5, 3);")))

; Test return statement without value
(define test-return-void
  (lambda ()
    (displayln "=== Testing Return Statement (void) ===")
    (test-parse-and-interpret "int test() { return; };")))

; Test nested function calls
(define test-nested-function-calls
  (lambda ()
    (displayln "=== Testing Nested Function Calls ===")
    (test-parse-and-interpret "int getValue() { return 10; }; int double(int x) { return x * 2; }; double(getValue());")))

; Run all function tests
(define run-function-tests
  (lambda ()
    (displayln "Starting Function Tests...")
    (displayln "")
    
    ; Basic function declaration tests
    (test-simple-function)
    (displayln "")
    
    (test-void-function)
    (displayln "")
    
    (test-function-with-params)
    (displayln "")
    
    ; Function call tests
    (test-function-call)
    (displayln "")
    
    (test-function-call-assignment)
    (displayln "")
    
    (test-function-call-with-args)
    (displayln "")
    
    (test-return-void)
    (displayln "")
    
    (test-nested-function-calls)
    (displayln "")
    
    (displayln "Function tests completed!")))

; Run the tests when this file is executed
(run-function-tests)
