#lang racket

(require "interpreter.rkt")
(require "lexer.rkt")
(require "parser.rkt")

; Simple debug function
(define debug-parse-and-interpret
  (lambda (input)
    (displayln (string-append "=== Testing: " input " ==="))
    (let* ((input-port (open-input-string input))
           (parsed (full-parser (lambda () (full-lexer input-port)))))
      (displayln (string-append "Parsed AST: " (format "~s" parsed)))
      (if parsed
          (let ((result (value-of-program parsed)))
            (displayln (string-append "Result: " (format "~s" result)))
            result)
          (displayln "Parse failed")))))

; Test simple cases step by step
(debug-parse-and-interpret "int i = 0;")
(debug-parse-and-interpret "$print(\"hello\");")
(debug-parse-and-interpret "int i = 0; $print(i);")
(debug-parse-and-interpret "int i = 0; i = i + 1; $print(i);")
(debug-parse-and-interpret "while (false) { $print(\"never\"); }")
(debug-parse-and-interpret "int i = 0; while (i < 3) { $print(i); i = i + 1; }")
(debug-parse-and-interpret "while (true) { $print(\"once\"); break; }")
