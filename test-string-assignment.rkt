#lang racket

(require "lexer.rkt")
(require "parser.rkt")
(require "interpreter.rkt")

; Test function to parse and interpret code
(define (test-parse-and-interpret code)
  (with-handlers
    [(exn:fail? (lambda (e)
                  (printf "Error: ~a\n" (exn-message e))
                  #f))]
    (let* ((tokens (tokenize-string code))
           (ast (parse-tokens tokens))
           (result (value-of-program ast)))
      (printf "Code: ~a\n" code)
      (printf "Result: ~a\n" result)
      result)))

; Test string assignment
(printf "=== Testing String Assignment ===\n")

; Test 1: Simple string assignment and printing
(test-parse-and-interpret "
{
  string s = \"felan\";
  $print(s);
}
")

; Test 2: String assignment and tocharlist
(test-parse-and-interpret "
{
  string s = \"hello\";
  $print($tocharlist(s));
}
")

; Test 3: String comparison
(test-parse-and-interpret "
{
  string s1 = \"test\";
  string s2 = \"test\";
  $print(s1 == s2);
}
")
