#lang racket

(require "lexer.rkt")
(require "parser.rkt") 
(require "interpreter.rkt")

; Test function for break and continue functionality
(define test-break-continue
  (lambda (code)
    (printf "=== Testing: ~a ===~n" code)
    (let* ((input-port (open-input-string code))
           (ast (full-parser (lambda () (full-lexer input-port))))
           (result (value-of-program ast)))
      (printf "Result: ~a~n" result)
      (printf "~n"))))

; Test cases for break and continue
(printf "Testing break and continue functionality...~n")

; Test 1: Basic break
(test-break-continue "int i = 0; while (true) { $print(i); i = i + 1; if (i >= 3) { break; } }")

; Test 2: Basic continue
(test-break-continue "int i = 0; while (i < 5) { i = i + 1; if (i % 2 == 0) { continue; } $print(i); }")

; Test 3: Nested loop with break (break should only exit inner loop)
(test-break-continue "int outer = 0; while (outer < 2) { $print(outer * 10); int inner = 0; while (inner < 5) { inner = inner + 1; if (inner == 2) { break; } $print(outer * 10 + inner); } outer = outer + 1; }")

; Test 4: Break and continue in same loop
(test-break-continue "int i = 0; while (i < 10) { i = i + 1; if (i == 3) { continue; } if (i == 7) { break; } $print(i); }")

(printf "Break and continue tests completed!~n")
