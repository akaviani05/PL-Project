#lang racket

(require "interpreter.rkt")
(require "lexer.rkt")
(require "parser.rkt")

; Test helper function
(define test-program
  (lambda (input-str expected-output)
    (display (string-append "Testing: " input-str "\n"))
    (let* ((input-port (open-input-string input-str))
           (ast (full-parser (lambda () (full-lexer input-port)))))
      (display "AST: ") (display ast) (newline)
      (let ((result (value-of-program ast)))
        (display "Result: ") (display result) (newline)
        (newline)))))

; Test $print statement
(display "=== Testing $print statement ===\n")
(test-program "$print(42);" "Should print 42")
(test-program "$print(5 + 3);" "Should print 8")
(test-program "$print(true);" "Should print true")
(test-program "$print(\"hello\");" "Should print hello")

; Test variable declarations and $print
(display "=== Testing variables with $print ===\n")
(test-program "int x = 10; $print(x);" "Should print 10")
(test-program "bool flag = true; $print(flag);" "Should print true")

; Test multiple statements
(display "=== Testing multiple statements ===\n")
(test-program "int a = 5; int b = 3; $print(a + b);" "Should print 8")

; Note: $input tests would require interactive input, so they're commented out
; but the implementation should work for:
; $input(x);  - reads input into variable x
