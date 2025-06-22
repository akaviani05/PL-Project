#lang racket

(require "interpreter.rkt")
(require "lexer.rkt")
(require "parser.rkt")

; Test function that uses the lexer and parser
(define test-parse-and-interpret
  (lambda (source-code)
    (displayln (format "Testing: ~s" source-code))
    (let* ((input-port (open-input-string source-code))
           (ast (full-parser (lambda () (full-lexer input-port)))))
      ;;; (displayln (format "AST: ~s" ast))
      (let ((result (value-of-program ast)))
        (displayln (format "Result: ~s" result))
        result))))

; Run basic tests with parser
(define run-basic-tests
  (lambda ()
    (displayln "=== Testing Basic Arithmetic ===")
    
    ; Simple addition
    (test-parse-and-interpret "5 + 3;")
    (displayln "")
    
    ; Simple subtraction
    (test-parse-and-interpret "10 - 4;")
    (displayln "")
    
    ; Multiplication
    (test-parse-and-interpret "6 * 7;")
    (displayln "")
    
    ; Division
    (test-parse-and-interpret "20 / 4;")
    (displayln "")
    
    ; Modulo
    (test-parse-and-interpret "17 % 5;")
    (displayln "")
    
    ; More complex expression with operator precedence
    (test-parse-and-interpret "2 + 3 * 4;")
    (displayln "")
    
    ; Expression with parentheses
    (test-parse-and-interpret "(2 + 3) * 4;")
    (displayln "")
    
    ; Mixed operations
    (test-parse-and-interpret "15 + 7 - 3 * 2;")
    (displayln "")))

; Test unary operators
(define run-unary-tests
  (lambda ()
    (displayln "=== Testing Unary Operators ===")
    
    ; Bitwise NOT
    (test-parse-and-interpret "~5;")
    (displayln "")
    
    ; Logical NOT with true
    (test-parse-and-interpret "!true;")
    (displayln "")
    
    ; Logical NOT with false
    (test-parse-and-interpret "!false;")
    (displayln "")))

; Test comparison operators (Op4)
(define run-comparison-tests
  (lambda ()
    (displayln "=== Testing Comparison Operators ===")
    
    ; Less than
    (test-parse-and-interpret "5 < 10;")
    (displayln "")
    
    (test-parse-and-interpret "10 < 5;")
    (displayln "")
    
    ; Greater than
    (test-parse-and-interpret "10 > 5;")
    (displayln "")
    
    (test-parse-and-interpret "5 > 10;")
    (displayln "")
    
    ; Less than or equal
    (test-parse-and-interpret "5 <= 10;")
    (displayln "")
    
    (test-parse-and-interpret "10 <= 10;")
    (displayln "")
    
    (test-parse-and-interpret "15 <= 10;")
    (displayln "")
    
    ; Greater than or equal
    (test-parse-and-interpret "10 >= 5;")
    (displayln "")
    
    (test-parse-and-interpret "10 >= 10;")
    (displayln "")
    
    (test-parse-and-interpret "5 >= 10;")
    (displayln "")
    
    ; Equality
    (test-parse-and-interpret "5 == 5;")
    (displayln "")
    
    (test-parse-and-interpret "5 == 10;")
    (displayln "")
    
    (test-parse-and-interpret "true == true;")
    (displayln "")
    
    (test-parse-and-interpret "true == false;")
    (displayln "")
    
    ; Inequality
    (test-parse-and-interpret "5 != 10;")
    (displayln "")
    
    (test-parse-and-interpret "5 != 5;")
    (displayln "")))

; Test bitwise operators (Op5)
(define run-bitwise-tests
  (lambda ()
    (displayln "=== Testing Bitwise Operators ===")
    
    ; Bitwise AND
    (test-parse-and-interpret "5 & 3;")  ; 5 = 101, 3 = 011, result = 001 = 1
    (displayln "")
    
    (test-parse-and-interpret "12 & 10;")  ; 12 = 1100, 10 = 1010, result = 1000 = 8
    (displayln "")
    
    ; Bitwise XOR
    (test-parse-and-interpret "5 ^ 3;")  ; 5 = 101, 3 = 011, result = 110 = 6
    (displayln "")
    
    (test-parse-and-interpret "12 ^ 10;")  ; 12 = 1100, 10 = 1010, result = 0110 = 6
    (displayln "")
    
    ; Bitwise OR
    (test-parse-and-interpret "5 | 3;")  ; 5 = 101, 3 = 011, result = 111 = 7
    (displayln "")
    
    (test-parse-and-interpret "12 | 10;")  ; 12 = 1100, 10 = 1010, result = 1110 = 14
    (displayln "")))

; Test logical operators (Op6 and Op7)
(define run-logical-tests
  (lambda ()
    (displayln "=== Testing Logical Operators ===")
    
    ; Logical AND (&&)
    (test-parse-and-interpret "true && true;")
    (displayln "")
    
    (test-parse-and-interpret "true && false;")
    (displayln "")
    
    (test-parse-and-interpret "false && true;")
    (displayln "")
    
    (test-parse-and-interpret "false && false;")
    (displayln "")
    
    ; Logical OR (||)
    (test-parse-and-interpret "true || true;")
    (displayln "")
    
    (test-parse-and-interpret "true || false;")
    (displayln "")
    
    (test-parse-and-interpret "false || true;")
    (displayln "")
    
    (test-parse-and-interpret "false || false;")
    (displayln "")))

; Test complex expressions with multiple operators
(define run-complex-tests
  (lambda ()
    (displayln "=== Testing Complex Expressions ===")
    
    ; Mixed arithmetic and comparison
    (test-parse-and-interpret "5 + 3 > 7;")
    (displayln "")
    
    (test-parse-and-interpret "10 - 5 == 5;")
    (displayln "")
    
    ; Mixed logical and comparison
    (test-parse-and-interpret "5 > 3 && 10 < 15;")
    (displayln "")
    
    (test-parse-and-interpret "5 > 10 || 3 < 7;")
    (displayln "")
    
    ; Complex nested expression
    (test-parse-and-interpret "(5 + 3) * 2 > 15 && true;")
    (displayln "")
    
    ; Bitwise with arithmetic
    (test-parse-and-interpret "(8 + 4) & (10 - 2);")
    (displayln "")))

; Test edge cases
(define run-edge-case-tests
  (lambda ()
    (displayln "=== Testing Edge Cases ===")
    
    ; Zero operations
    (test-parse-and-interpret "0 + 5;")
    (displayln "")
    
    (test-parse-and-interpret "5 * 0;")
    (displayln "")
    
    ; Negative numbers
    (test-parse-and-interpret "10 - 15;")
    (displayln "")
    
    ; Complex nested operations
    (test-parse-and-interpret "((10 + 5) * 2) / 3;")
    (displayln "")
    
    ; Short-circuit evaluation test
    (test-parse-and-interpret "false && true;")  ; Should short-circuit
    (displayln "")
    
    (test-parse-and-interpret "true || false;")  ; Should short-circuit
    (displayln "")))

; Test variable declarations
(define run-variable-tests
  (lambda ()
    (displayln "=== Testing Variable Declarations ===")
    
    ; Variable declaration with initialization
    (test-parse-and-interpret "int x = 5;")
    (displayln "")
    
    ; Variable declaration without initialization
    (test-parse-and-interpret "int y;")
    (displayln "")))

; Run tests for $print statement
(define run-print-tests
  (lambda ()
    (displayln "=== Testing $print Statement ===")
    
    ; Test printing simple values
    (test-parse-and-interpret "$print(42);")
    (displayln "")
    
    (test-parse-and-interpret "$print(3.14);")
    (displayln "")
    
    (test-parse-and-interpret "$print(true);")
    (displayln "")
    
    (test-parse-and-interpret "$print(false);")
    (displayln "")
    
    (test-parse-and-interpret "$print(\"hello world\");")
    (displayln "")
    
    (test-parse-and-interpret "$print('c');")
    (displayln "")
    
    ; Test printing expressions
    (test-parse-and-interpret "$print(5 + 3);")
    (displayln "")
    
    (test-parse-and-interpret "$print(10 * 2 - 5);")
    (displayln "")
    
    (test-parse-and-interpret "$print(20 / 4);")
    (displayln "")
    
    (test-parse-and-interpret "$print(17 % 5);")
    (displayln "")
    
    ; Test printing comparisons
    (test-parse-and-interpret "$print(5 > 3);")
    (displayln "")
    
    (test-parse-and-interpret "$print(2 == 2);")
    (displayln "")
    
    (test-parse-and-interpret "$print(7 <= 5);")
    (displayln "")
    
    ; Test printing variables
    (test-parse-and-interpret "int x = 100; $print(x);")
    (displayln "")
    
    (test-parse-and-interpret "bool flag = true; $print(flag);")
    (displayln "")
    
    (test-parse-and-interpret "string msg = \"test\"; $print(msg);")
    (displayln "")
    
    ; Test multiple prints
    (test-parse-and-interpret "$print(1); $print(2); $print(3);")
    (displayln "")
    
    (displayln "=== $print Tests Completed ===")
    (displayln "")))

; Test if statements
(define run-if-tests
  (lambda ()
    (displayln "=== Testing If Statements ===")
    
    ; Simple if statement - true condition
    (test-parse-and-interpret "if (5 > 3) { $print(\"condition is true\"); }")
    (displayln "")
    
    ; Simple if statement - false condition
    (test-parse-and-interpret "if (3 > 5) { $print(\"this should not print\"); }")
    (displayln "")
    
    ; If-else statement - true condition
    (test-parse-and-interpret "if (10 == 10) { $print(\"equal\"); } else { $print(\"not equal\"); }")
    (displayln "")
    
    ; If-else statement - false condition
    (test-parse-and-interpret "if (10 == 5) { $print(\"equal\"); } else { $print(\"not equal\"); }")
    (displayln "")
    
    ; Nested if statements
    (test-parse-and-interpret "if (true) { if (5 < 10) { $print(\"nested condition true\"); } }")
    (displayln "")
    
    ; If with variable conditions
    (test-parse-and-interpret "int x = 15; if (x > 10) { $print(\"x is greater than 10\"); }")
    (displayln "")
    
    ; If-else with complex expressions
    (test-parse-and-interpret "int a = 8; int b = 12; if (a + b > 15) { $print(\"sum is large\"); } else { $print(\"sum is small\"); }")
    (displayln "")
    
    ; If-elseif chain
    (test-parse-and-interpret "int score = 85; if (score >= 90) { $print(\"A grade\"); } else if (score >= 80) { $print(\"B grade\"); } else { $print(\"C grade\"); }")
    (displayln "")
    
    ; Multiple if-elseif
    (test-parse-and-interpret "int num = 0; if (num > 0) { $print(\"positive\"); } else if (num < 0) { $print(\"negative\"); } else { $print(\"zero\"); }")
    (displayln "")
    
    ; If with boolean variables
    (test-parse-and-interpret "bool flag = true; if (flag) { $print(\"flag is true\"); } else { $print(\"flag is false\"); }")
    (displayln "")
    
    (displayln "=== If Statement Tests Completed ===")
    (displayln "")))

; Test while statements
(define run-while-tests
  (lambda ()
    (displayln "=== Testing While Statements ===")
    
    ; Simple while loop
    (test-parse-and-interpret "int i = 0; while (i < 3) { $print(i); i = i + 1; }")
    (displayln "")
    
    ; While loop with different condition
    (test-parse-and-interpret "int count = 5; while (count > 0) { $print(count); count = count - 1; }")
    (displayln "")
    
    ; While loop that doesn't execute
    (test-parse-and-interpret "int x = 10; while (x < 5) { $print(\"this should not print\"); }")
    (displayln "")
    
    ; While loop with boolean condition
    (test-parse-and-interpret "bool running = true; int counter = 0; while (running) { $print(counter); counter = counter + 1; if (counter >= 3) { running = false; } }")
    (displayln "")
    
    ; Nested while loops
    (test-parse-and-interpret "int outer = 0; while (outer < 2) { int inner = 0; while (inner < 2) { $print(outer * 10 + inner); inner = inner + 1; } outer = outer + 1; }")
    (displayln "")
    
    ; While with complex condition
    (test-parse-and-interpret "int a = 1; int b = 10; while (a < b && a < 5) { $print(a); a = a + 1; }")
    (displayln "")
    
    ; Sum calculation with while
    (test-parse-and-interpret "int sum = 0; int n = 1; while (n <= 4) { sum = sum + n; n = n + 1; } $print(sum);")
    (displayln "")
    
    (displayln "=== While Statement Tests Completed ===")
    (displayln "")))

; Test control flow combinations
(define run-control-flow-tests
  (lambda ()
    (displayln "=== Testing Control Flow Combinations ===")
    
    ; If inside while
    (test-parse-and-interpret "int i = 0; while (i < 5) { if (i % 2 == 0) { $print(\"even: \"); $print(i); } i = i + 1; }")
    (displayln "")
    
    ; While inside if
    (test-parse-and-interpret "bool condition = true; if (condition) { int j = 0; while (j < 3) { $print(j); j = j + 1; } }")
    (displayln "")
    
    ; Multiple control structures
    (test-parse-and-interpret "int x = 8; if (x > 5) { int k = 0; while (k < x - 5) { $print(k); k = k + 1; } } else { $print(\"x is too small\"); }")
    (displayln "")
    
    ; Complex nested structure
    (test-parse-and-interpret "int limit = 3; int i = 0; while (i < limit) { if (i == 1) { $print(\"middle\"); } else if (i == 0) { $print(\"start\"); } else { $print(\"end\"); } i = i + 1; }")
    (displayln "")
    
    (displayln "=== Control Flow Combination Tests Completed ===")
    (displayln "")))

; Test $tocharlist statement
(define run-tocharlist-tests
  (lambda ()
    (displayln "=== Testing $tocharlist Statement ===")
    
    ; Test converting simple string to char list
    (test-parse-and-interpret "string s = \"abc\"; list<char> chars = $tocharlist(s); $print(chars);")
    (displayln "")
    
    ; Test with empty string
    (test-parse-and-interpret "string empty = \"\"; list<char> emptyChars = $tocharlist(empty); $print(emptyChars);")
    (displayln "")
    
    ; Test with single character string
    (test-parse-and-interpret "string single = \"x\"; list<char> singleChar = $tocharlist(single); $print(singleChar);")
    (displayln "")
    
    ; Test with longer string
    (test-parse-and-interpret "string message = \"Hello World\"; list<char> messageChars = $tocharlist(message); $print(messageChars);")
    (displayln "")
    
    ; Test with special characters
    (test-parse-and-interpret "string special = \"123!@#\"; list<char> specialChars = $tocharlist(special); $print(specialChars);")
    (displayln "")
    
    ; Test direct usage in expression
    (test-parse-and-interpret "$print($tocharlist(\"test\"));")
    (displayln "")
    
    ; Test with string variable
    (test-parse-and-interpret "string text = \"racket\"; $print($tocharlist(text));")
    (displayln "")
    
    ; Test accessing individual characters from the list
    (test-parse-and-interpret "string word = \"hi\"; list<char> letters = $tocharlist(word); $print($get(letters, 0)); $print($get(letters, 1));")
    (displayln "")
    
    ; Test modifying the char list
    (test-parse-and-interpret "string original = \"abc\"; list<char> chars = $tocharlist(original); $set(chars, 1, 'X'); $print(chars);")
    (displayln "")
    
    ; Test adding chars to the list
    (test-parse-and-interpret "string base = \"ab\"; list<char> chars = $tocharlist(base); $push(chars, 'c'); $print(chars);")
    (displayln "")
    
    (displayln "=== $tocharlist Tests Completed ===")
    (displayln "")))

; Updated run-all-tests function
(define run-all-tests
  (lambda ()
    (displayln "Starting comprehensive tests...")
    (run-basic-tests)
    (run-unary-tests)
    (run-comparison-tests)
    (run-bitwise-tests)
    (run-logical-tests)
    (run-complex-tests)
    (run-edge-case-tests)
    (run-variable-tests)
    (run-print-tests)
    (run-if-tests)         
    (run-while-tests)      
    (run-control-flow-tests) 
    (run-tocharlist-tests)  ; Add tocharlist tests
    (displayln "All tests completed!")))

(run-all-tests)
(provide run-all-tests run-basic-tests run-unary-tests run-comparison-tests 
         run-bitwise-tests run-logical-tests run-complex-tests 
         run-edge-case-tests run-variable-tests run-print-tests 
         run-if-tests run-while-tests run-control-flow-tests run-tocharlist-tests test-parse-and-interpret)
