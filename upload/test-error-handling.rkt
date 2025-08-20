#lang racket

(require "interpreter.rkt")
(require "lexer.rkt")
(require "parser.rkt")

(define test-parse-and-interpret
  (lambda (source-code)
    (displayln (format "Testing: ~s" source-code))
    (with-handlers 
      ([exn:fail? (lambda (e) 
                    (displayln (format "Error: ~a" (exn-message e)))
                    (void))])
      (let* ((input-port (open-input-string source-code))
             (ast (full-parser (lambda () (full-lexer input-port)))))
        (let ((result (value-of-program ast)))
          (displayln (format "Result: ~s" result))
          result)))))

(define run-basic-tests
  (lambda ()
    (displayln "* Testing Undefined Variables *")
    
    (test-parse-and-interpret "int x = 10; $print(y);")
      (displayln "* Testing Type errors *")
    (test-parse-and-interpret "int x = 10; string a = \"Hello, World!\"; $print(x + a);")
    
    (displayln "")
    (displayln "* Testing Division by Zero *")
    (test-parse-and-interpret "int b = 10 / 0;")
    
    (displayln "")
    (displayln "* Testing Function Arity Dismatch *")
    (test-parse-and-interpret "int add(int x, int y) { return x + y; }; $print(add(5));")

    (displayln "")
    (displayln "Index out of range")
    (test-parse-and-interpret "list a; $push(a, 0); $push(a, 1); $print($get(a, 0)); $print($get(a, 2));")
))

(define run-all-tests
  (lambda ()
    (run-basic-tests)))

(run-all-tests)
