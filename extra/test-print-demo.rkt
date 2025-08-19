#lang racket

(require "interpreter.rkt")
(require "lexer.rkt")
(require "parser.rkt")

; Simple test function for demonstrations
(define test-print
  (lambda (source-code)
    (displayln (format "Code: ~s" source-code))
    (let* ((input-port (open-input-string source-code))
           (ast (full-parser (lambda () (full-lexer input-port)))))
      (let ((result (value-of-program ast)))
        (displayln (format "Returns: ~s" result))
        (displayln "")))))

(displayln "=== $print Statement Demonstration ===")
(displayln "")

; Basic value printing
(displayln "1. Printing basic values:")
(test-print "$print(42);")
(test-print "$print(3.14);")
(test-print "$print(true);")
(test-print "$print(\"Hello, World!\");")

; Expression printing
(displayln "2. Printing expressions:")
(test-print "$print(10 + 5);")
(test-print "$print(20 - 8);")
(test-print "$print(6 * 7);")
(test-print "$print(100 / 4);")
(test-print "$print(17 % 5);")

; Comparison printing
(displayln "3. Printing comparisons:")
(test-print "$print(5 > 3);")
(test-print "$print(10 == 10);")
(test-print "$print(7 <= 5);")

; Variable printing
(displayln "4. Printing variables:")
(test-print "int x = 25; $print(x);")
(test-print "bool flag = false; $print(flag);")
(test-print "string message = \"Test message\"; $print(message);")

; Complex expressions
(displayln "5. Printing complex expressions:")
(test-print "$print((5 + 3) * 2);")
(test-print "$print(10 > 5 && 3 < 7);")
(test-print "int a = 10; int b = 20; $print(a + b * 2);")

(displayln "=== Demonstration Complete ===")
