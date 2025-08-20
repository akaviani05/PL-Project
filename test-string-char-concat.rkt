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
    (let* ((input-port (open-input-string code))
           (ast (full-parser (lambda () (full-lexer input-port))))
           (result (value-of-program ast)))
      (printf "Code: ~a\n" code)
      (printf "Result: ~a\n" result)
      result)))

; Test string + char concatenation
(printf "=== Testing String + Char Concatenation ===\n")

; Test 1: Simple string + char
(test-parse-and-interpret "
{
  string s = \"hello\";
  char c = 'x';
  $print(s + c);
}
")

; Test 2: Direct string + char literal
(test-parse-and-interpret "
{
  $print(\"test\" + 'y');
}
")

; Test 3: Multiple concatenations
(test-parse-and-interpret "
{
  string word = \"wor\";
  char l1 = 'l';
  char d = 'd';
  $print(word + l1 + d);
}
")

; Test 4: Using in assignment
(test-parse-and-interpret "
{
  string base = \"abc\";
  string result = base + 'd';
  $print(result);
}
")

; Test 5: Test error case - char + string (should fail)
(test-parse-and-interpret "
{
  char c = 'x';
  string s = \"hello\";
  $print(c + s);
}
")

; Test 6: Test error case - string + number (should fail)
(test-parse-and-interpret "
{
  string s = \"hello\";
  $print(s + 5);
}
")

; Test 7: Test mixed with regular arithmetic
(test-parse-and-interpret "
{
  string s = \"result: \";
  int num = 3 + 4;
  char c = 'x';
  $print(s + c);
  $print(num);
}
")
