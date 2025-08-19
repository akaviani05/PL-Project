#lang racket

(require "interpreter.rkt")
(require "lexer.rkt")
(require "parser.rkt")

(define test-parse-and-interpret
  (lambda (source-code)
    (displayln (format "Testing: ~s" source-code))
    (let* ((input-port (open-input-string source-code))
           (ast (full-parser (lambda () (full-lexer input-port)))))
      ;;; (displayln (format "AST: ~s" ast))
      (let ((result (value-of-program ast)))
        (displayln (format "Result: ~s" result))
        result))))


(test-parse-and-interpret "string single = \"x\"; list singleChar = $tocharlist(single); $print(singleChar);")

(provide test-parse-and-interpret)