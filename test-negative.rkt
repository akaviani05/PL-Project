#lang racket

(require "interpreter.rkt")
(require "lexer.rkt")
(require "parser.rkt")
(require "datatypes.rkt")

; Test negative number workaround
(define run-demo
  (lambda (description source-code)
    (displayln (format "=== ~a ===" description))
    (displayln (format "Code: ~a" source-code))
    (displayln "Output:")
    (let* ((input-port (open-input-string source-code))
           (ast (full-parser (lambda () (full-lexer input-port)))))
      (let ((result (value-of-program ast)))
        (displayln (format "Final Result: ~s" result))
        result))))

(run-demo "Test Absolute Value Function"
          "int abs(int x) { if (x < 0) { return 0 - x; } else { return x; } }; int negativeNum = 0 - 15; abs(negativeNum);")
