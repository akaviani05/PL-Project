#lang racket

(require "interpreter.rkt")
(require "lexer.rkt")
(require "parser.rkt")

; Test function that uses the lexer and parser
(define test-parse-and-interpret
  (lambda (source-code)
    (displayln (format "Testing: ~s" source-code))
    (let* ((input-port (open-input-string source-code))
           (ast (full-parser (lambda () (full-lexer input-port)))))
      ;;; (displayln (format "AST: ~s" ast))
      (let ((result (value-of-program ast)))
        (displayln (format "Result: ~s" result))
        result))))

; Run basic tests with parser
(define run-basic-tests
  (lambda ()
    (displayln "=== Testing Basic Arithmetic ===")
    
    ; Simple addition
    (test-parse-and-interpret "5 + 3;")
    (displayln "")
    
    ; Simple subtraction
    (test-parse-and-interpret "10 - 4;")
    (displayln "")
    
    ; Multiplication
    (test-parse-and-interpret "6 * 7;")
    (displayln "")
    
    ; Division
    (test-parse-and-interpret "20 / 4;")
    (displayln "")
    
    ; Modulo
    (test-parse-and-interpret "17 % 5;")
    (displayln "")
    
    ; More complex expression with operator precedence
    (test-parse-and-interpret "2 + 3 * 4;")
    (displayln "")
    
    ; Expression with parentheses
    (test-parse-and-interpret "(2 + 3) * 4;")
    (displayln "")
    
    ; Mixed operations
    (test-parse-and-interpret "15 + 7 - 3 * 2;")
    (displayln "")))

; Test unary operators
(define run-unary-tests
  (lambda ()
    (displayln "=== Testing Unary Operators ===")
    
    ; Bitwise NOT
    (test-parse-and-interpret "~5;")
    (displayln "")
    
    ; Logical NOT with true
    (test-parse-and-interpret "!true;")
    (displayln "")
    
    ; Logical NOT with false
    (test-parse-and-interpret "!false;")
    (displayln "")))

; Test comparison operators (Op4)
(define run-comparison-tests
  (lambda ()
    (displayln "=== Testing Comparison Operators ===")
    
    ; Less than
    (test-parse-and-interpret "5 < 10;")
    (displayln "")
    
    (test-parse-and-interpret "10 < 5;")
    (displayln "")
    
    ; Greater than
    (test-parse-and-interpret "10 > 5;")
    (displayln "")
    
    (test-parse-and-interpret "5 > 10;")
    (displayln "")
    
    ; Less than or equal
    (test-parse-and-interpret "5 <= 10;")
    (displayln "")
    
    (test-parse-and-interpret "10 <= 10;")
    (displayln "")
    
    (test-parse-and-interpret "15 <= 10;")
    (displayln "")
    
    ; Greater than or equal
    (test-parse-and-interpret "10 >= 5;")
    (displayln "")
    
    (test-parse-and-interpret "10 >= 10;")
    (displayln "")
    
    (test-parse-and-interpret "5 >= 10;")
    (displayln "")
    
    ; Equality
    (test-parse-and-interpret "5 == 5;")
    (displayln "")
    
    (test-parse-and-interpret "5 == 10;")
    (displayln "")
    
    (test-parse-and-interpret "true == true;")
    (displayln "")
    
    (test-parse-and-interpret "true == false;")
    (displayln "")
    
    ; Inequality
    (test-parse-and-interpret "5 != 10;")
    (displayln "")
    
    (test-parse-and-interpret "5 != 5;")
    (displayln "")))

; Test bitwise operators (Op5)
(define run-bitwise-tests
  (lambda ()
    (displayln "=== Testing Bitwise Operators ===")
    
    ; Bitwise AND
    (test-parse-and-interpret "5 & 3;")  ; 5 = 101, 3 = 011, result = 001 = 1
    (displayln "")
    
    (test-parse-and-interpret "12 & 10;")  ; 12 = 1100, 10 = 1010, result = 1000 = 8
    (displayln "")
    
    ; Bitwise XOR
    (test-parse-and-interpret "5 ^ 3;")  ; 5 = 101, 3 = 011, result = 110 = 6
    (displayln "")
    
    (test-parse-and-interpret "12 ^ 10;")  ; 12 = 1100, 10 = 1010, result = 0110 = 6
    (displayln "")
    
    ; Bitwise OR
    (test-parse-and-interpret "5 | 3;")  ; 5 = 101, 3 = 011, result = 111 = 7
    (displayln "")
    
    (test-parse-and-interpret "12 | 10;")  ; 12 = 1100, 10 = 1010, result = 1110 = 14
    (displayln "")))

; Test logical operators (Op6 and Op7)
(define run-logical-tests
  (lambda ()
    (displayln "=== Testing Logical Operators ===")
    
    ; Logical AND (&&)
    (test-parse-and-interpret "true && true;")
    (displayln "")
    
    (test-parse-and-interpret "true && false;")
    (displayln "")
    
    (test-parse-and-interpret "false && true;")
    (displayln "")
    
    (test-parse-and-interpret "false && false;")
    (displayln "")
    
    ; Logical OR (||)
    (test-parse-and-interpret "true || true;")
    (displayln "")
    
    (test-parse-and-interpret "true || false;")
    (displayln "")
    
    (test-parse-and-interpret "false || true;")
    (displayln "")
    
    (test-parse-and-interpret "false || false;")
    (displayln "")))

; Test complex expressions with multiple operators
(define run-complex-tests
  (lambda ()
    (displayln "=== Testing Complex Expressions ===")
    
    ; Mixed arithmetic and comparison
    (test-parse-and-interpret "5 + 3 > 7;")
    (displayln "")
    
    (test-parse-and-interpret "10 - 5 == 5;")
    (displayln "")
    
    ; Mixed logical and comparison
    (test-parse-and-interpret "5 > 3 && 10 < 15;")
    (displayln "")
    
    (test-parse-and-interpret "5 > 10 || 3 < 7;")
    (displayln "")
    
    ; Complex nested expression
    (test-parse-and-interpret "(5 + 3) * 2 > 15 && true;")
    (displayln "")
    
    ; Bitwise with arithmetic
    (test-parse-and-interpret "(8 + 4) & (10 - 2);")
    (displayln "")))

; Test edge cases
(define run-edge-case-tests
  (lambda ()
    (displayln "=== Testing Edge Cases ===")
    
    ; Zero operations
    (test-parse-and-interpret "0 + 5;")
    (displayln "")
    
    (test-parse-and-interpret "5 * 0;")
    (displayln "")
    
    ; Negative numbers
    (test-parse-and-interpret "10 - 15;")
    (displayln "")
    
    ; Complex nested operations
    (test-parse-and-interpret "((10 + 5) * 2) / 3;")
    (displayln "")
    
    ; Short-circuit evaluation test
    (test-parse-and-interpret "false && true;")  ; Should short-circuit
    (displayln "")
    
    (test-parse-and-interpret "true || false;")  ; Should short-circuit
    (displayln "")))

; Test variable declarations
(define run-variable-tests
  (lambda ()
    (displayln "=== Testing Variable Declarations ===")
    
    ; Variable declaration with initialization
    (test-parse-and-interpret "int x = 5;")
    (displayln "")
    
    ; Variable declaration without initialization
    (test-parse-and-interpret "int y;")
    (displayln "")))

; Run all tests
(define run-all-tests
  (lambda ()
    (displayln "Starting interpreter tests with parser...")
    (run-basic-tests)
    (run-unary-tests)
    (run-comparison-tests)
    (run-bitwise-tests)
    (run-logical-tests)
    (run-complex-tests)
    (run-edge-case-tests)
    (run-variable-tests)
    (displayln "All tests completed!")))

(run-all-tests)
(provide run-all-tests run-basic-tests run-unary-tests run-comparison-tests 
         run-bitwise-tests run-logical-tests run-complex-tests 
         run-edge-case-tests run-variable-tests test-parse-and-interpret)
