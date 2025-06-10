#lang racket

(require "interpreter.rkt")
(require "lexer.rkt")
(require "parser.rkt")

; Helper function to see the AST structure
(define debug-parse
  (lambda (source-code)
    (displayln (format "Source: ~s" source-code))
    (let* ((input-port (open-input-string source-code))
           (ast (full-parser (lambda () (full-lexer input-port)))))
      (displayln (format "AST: ~s" ast))
      ast)))

; Debug function call structure
(displayln "=== Debugging Function Call Structure ===")
(debug-parse "getValue();")

(displayln "\n=== Debugging Function Declaration (no params) ===")
(debug-parse "int getValue() { return 42; };")

(displayln "\n=== Debugging Function Declaration (with params) ===")
(debug-parse "int add(int a, int b) { return a + b; };")

(displayln "\n=== Debugging Function Declaration and Call ===")
(debug-parse "int getValue() { return 42; }; getValue();")

(displayln "\n=== Debugging Function Call with Arguments ===")
(debug-parse "int add(int a, int b) { return a + b; }; add(5, 3);")
