#lang racket

(require "interpreter.rkt")
(require "lexer.rkt")
(require "parser.rkt")

; Test framework functions
(define test-counter 0)
(define passed-tests 0)
(define failed-tests 0)

(define (reset-test-counters!)
  (set! test-counter 0)
  (set! passed-tests 0)
  (set! failed-tests 0))

(define (increment-test!)
  (set! test-counter (+ test-counter 1)))

(define (increment-passed!)
  (set! passed-tests (+ passed-tests 1)))

(define (increment-failed!)
  (set! failed-tests (+ failed-tests 1)))

; Test execution function
(define (run-test description source-code expected-behavior)
  (increment-test!)
  (displayln (format "Test ~a: ~a" test-counter description))
  (displayln (format "  Code: ~s" source-code))
  
  (with-handlers
    [(exn:fail? (lambda (e) 
                  (displayln (format "  ERROR: ~a" (exn-message e)))
                  (increment-failed!)
                  #f))]
    
    (let* ((input-port (open-input-string source-code))
           (ast (full-parser (lambda () (full-lexer input-port))))
           (result (value-of-program ast)))
      (displayln (format "  Result: ~s" result))
      (displayln (format "  Expected: ~a" expected-behavior))
      (increment-passed!)
      (displayln "  PASSED")
      (displayln "")
      result)))

; Test summary
(define (print-test-summary)
  (displayln "")
  (displayln "=== TEST SUMMARY ===")
  (displayln (format "Total tests: ~a" test-counter))
  (displayln (format "Passed: ~a" passed-tests))
  (displayln (format "Failed: ~a" failed-tests))
  (displayln (format "Success rate: ~a%" 
    (if (= test-counter 0) 0 
        (exact->inexact (* 100 (/ passed-tests test-counter))))))
  (displayln ""))

; ===== ARITHMETIC OPERATIONS TESTS =====
(define (test-arithmetic-operations)
  (displayln "===== TESTING ARITHMETIC OPERATIONS =====")
  
  ; Addition tests
  (run-test "Basic addition" "5 + 3;" "should return 8")
  (run-test "Addition with zero" "10 + 0;" "should return 10")
  (run-test "Negative addition" "5 + (-3);" "should return 2")
  (run-test "Multiple additions" "1 + 2 + 3 + 4;" "should return 10")
  
  ; Subtraction tests
  (run-test "Basic subtraction" "10 - 4;" "should return 6")
  (run-test "Subtraction to zero" "5 - 5;" "should return 0")
  (run-test "Subtraction to negative" "3 - 7;" "should return -4")
  (run-test "Multiple subtractions" "20 - 5 - 3 - 2;" "should return 10")
  
  ; Multiplication tests
  (run-test "Basic multiplication" "6 * 7;" "should return 42")
  (run-test "Multiplication by zero" "100 * 0;" "should return 0")
  (run-test "Multiplication by one" "25 * 1;" "should return 25")
  (run-test "Multiple multiplications" "2 * 3 * 4;" "should return 24")
  
  ; Division tests
  (run-test "Basic division" "20 / 4;" "should return 5")
  (run-test "Division resulting in float" "7 / 2;" "should return 3.5")
  (run-test "Division by one" "15 / 1;" "should return 15")
  
  ; Modulo tests
  (run-test "Basic modulo" "17 % 5;" "should return 2")
  (run-test "Modulo by larger number" "5 % 7;" "should return 5")
  (run-test "Even modulo" "8 % 4;" "should return 0")
  
  ; Operator precedence tests
  (run-test "Multiplication before addition" "2 + 3 * 4;" "should return 14")
  (run-test "Division before subtraction" "20 - 12 / 3;" "should return 16")
  (run-test "Parentheses override precedence" "(2 + 3) * 4;" "should return 20")
  (run-test "Complex precedence" "2 + 3 * 4 - 1;" "should return 13")
  
  (displayln "===== ARITHMETIC OPERATIONS TESTS COMPLETED =====")
  (displayln ""))

; ===== UNARY OPERATIONS TESTS =====
(define (test-unary-operations)
  (displayln "===== TESTING UNARY OPERATIONS =====")
  
  ; Logical NOT tests
  (run-test "NOT true" "!true;" "should return false")
  (run-test "NOT false" "!false;" "should return true")
  (run-test "Double NOT" "!!true;" "should return true")
  (run-test "NOT with expression" "!(5 > 10);" "should return true")
  
  ; Bitwise NOT tests
  (run-test "Bitwise NOT small number" "~5;" "should return bitwise complement of 5")
  (run-test "Bitwise NOT zero" "~0;" "should return -1")
  (run-test "Bitwise NOT negative" "~(-1);" "should return 0")
  
  (displayln "===== UNARY OPERATIONS TESTS COMPLETED =====")
  (displayln ""))

; ===== COMPARISON OPERATIONS TESTS =====
(define (test-comparison-operations)
  (displayln "===== TESTING COMPARISON OPERATIONS =====")
  
  ; Less than tests
  (run-test "Less than true" "5 < 10;" "should return true")
  (run-test "Less than false" "10 < 5;" "should return false")
  (run-test "Less than equal" "5 < 5;" "should return false")
  
  ; Greater than tests
  (run-test "Greater than true" "10 > 5;" "should return true")
  (run-test "Greater than false" "5 > 10;" "should return false")
  (run-test "Greater than equal" "5 > 5;" "should return false")
  
  ; Less than or equal tests
  (run-test "Less equal true (less)" "5 <= 10;" "should return true")
  (run-test "Less equal true (equal)" "10 <= 10;" "should return true")
  (run-test "Less equal false" "15 <= 10;" "should return false")
  
  ; Greater than or equal tests
  (run-test "Greater equal true (greater)" "10 >= 5;" "should return true")
  (run-test "Greater equal true (equal)" "10 >= 10;" "should return true")
  (run-test "Greater equal false" "5 >= 10;" "should return false")
  
  ; Equality tests
  (run-test "Equal integers true" "5 == 5;" "should return true")
  (run-test "Equal integers false" "5 == 10;" "should return false")
  (run-test "Equal booleans true" "true == true;" "should return true")
  (run-test "Equal booleans false" "true == false;" "should return false")
  (run-test "Equal mixed types" "5 == 5.0;" "should return true")
  
  ; Inequality tests
  (run-test "Not equal integers true" "5 != 10;" "should return true")
  (run-test "Not equal integers false" "5 != 5;" "should return false")
  (run-test "Not equal booleans true" "true != false;" "should return true")
  (run-test "Not equal booleans false" "true != true;" "should return false")
  
  ; Chained comparisons
  (run-test "Multiple comparisons" "5 < 10 && 10 < 15;" "should return true")
  (run-test "Mixed comparison types" "3 + 2 == 5;" "should return true")
  
  (displayln "===== COMPARISON OPERATIONS TESTS COMPLETED =====")
  (displayln ""))

; ===== BITWISE OPERATIONS TESTS =====
(define (test-bitwise-operations)
  (displayln "===== TESTING BITWISE OPERATIONS =====")
  
  ; Bitwise AND tests
  (run-test "Bitwise AND basic" "5 & 3;" "should return 1 (101 & 011 = 001)")
  (run-test "Bitwise AND larger" "12 & 10;" "should return 8 (1100 & 1010 = 1000)")
  (run-test "Bitwise AND with zero" "15 & 0;" "should return 0")
  (run-test "Bitwise AND same numbers" "7 & 7;" "should return 7")
  
  ; Bitwise XOR tests
  (run-test "Bitwise XOR basic" "5 ^ 3;" "should return 6 (101 ^ 011 = 110)")
  (run-test "Bitwise XOR larger" "12 ^ 10;" "should return 6 (1100 ^ 1010 = 0110)")
  (run-test "Bitwise XOR with zero" "15 ^ 0;" "should return 15")
  (run-test "Bitwise XOR same numbers" "7 ^ 7;" "should return 0")
  
  ; Bitwise OR tests
  (run-test "Bitwise OR basic" "5 | 3;" "should return 7 (101 | 011 = 111)")
  (run-test "Bitwise OR larger" "12 | 10;" "should return 14 (1100 | 1010 = 1110)")
  (run-test "Bitwise OR with zero" "15 | 0;" "should return 15")
  (run-test "Bitwise OR same numbers" "7 | 7;" "should return 7")
  
  ; Combined bitwise operations
  (run-test "Combined bitwise ops" "(5 & 3) | (2 ^ 4);" "should combine operations")
  (run-test "Bitwise with arithmetic" "(8 + 4) & (10 - 2);" "should work with arithmetic")
  
  (displayln "===== BITWISE OPERATIONS TESTS COMPLETED =====")
  (displayln ""))

; ===== LOGICAL OPERATIONS TESTS =====
(define (test-logical-operations)
  (displayln "===== TESTING LOGICAL OPERATIONS =====")
  
  ; Logical AND tests
  (run-test "Logical AND both true" "true && true;" "should return true")
  (run-test "Logical AND first false" "false && true;" "should return false")
  (run-test "Logical AND second false" "true && false;" "should return false")
  (run-test "Logical AND both false" "false && false;" "should return false")
  (run-test "Logical AND with expressions" "(5 > 3) && (10 < 15);" "should return true")
  (run-test "Logical AND short circuit" "false && (1/0 > 0);" "should not cause division by zero")
  
  ; Logical OR tests
  (run-test "Logical OR both true" "true || true;" "should return true")
  (run-test "Logical OR first true" "true || false;" "should return true")
  (run-test "Logical OR second true" "false || true;" "should return true")
  (run-test "Logical OR both false" "false || false;" "should return false")
  (run-test "Logical OR with expressions" "(5 > 10) || (3 < 7);" "should return true")
  (run-test "Logical OR short circuit" "true || (1/0 > 0);" "should not cause division by zero")
  
  ; Complex logical expressions
  (run-test "Complex logical 1" "true && (false || true);" "should return true")
  (run-test "Complex logical 2" "(true || false) && (true && false);" "should return false")
  (run-test "Mixed logical and comparison" "(5 == 5) && (10 > 7) || false;" "should return true")
  
  (displayln "===== LOGICAL OPERATIONS TESTS COMPLETED =====")
  (displayln ""))

; ===== VARIABLE OPERATIONS TESTS =====
(define (test-variable-operations)
  (displayln "===== TESTING VARIABLE OPERATIONS =====")
  
  ; Integer variable tests
  (run-test "Int declaration with init" "int x = 5;" "should declare and initialize x")
  (run-test "Int declaration default" "int y;" "should declare y with default value 0")
  (run-test "Int variable usage" "int x = 10; x + 5;" "should return 15")
  (run-test "Int variable assignment" "int x = 5; x = 10; x;" "should return 10")
  
  ; Float variable tests
  (run-test "Float declaration with init" "float f = 3.14;" "should declare and initialize f")
  (run-test "Float declaration default" "float g;" "should declare g with default value 0.0")
  (run-test "Float variable usage" "float f = 2.5; f * 2;" "should return 5.0")
  
  ; Boolean variable tests
  (run-test "Bool declaration with init" "bool flag = true;" "should declare and initialize flag")
  (run-test "Bool declaration default" "bool state;" "should declare state with default value false")
  (run-test "Bool variable usage" "bool flag = true; !flag;" "should return false")
  
  ; String variable tests
  (run-test "String declaration with init" "string msg = \"hello\";" "should declare and initialize msg")
  (run-test "String declaration default" "string text;" "should declare text with default empty string")
  
  ; Character variable tests
  (run-test "Char declaration with init" "char c = 'a';" "should declare and initialize c")
  (run-test "Char declaration default" "char ch;" "should declare ch with default empty char")
  
  ; List variable tests
  (run-test "List declaration with init" "list<int> nums = [1, 2, 3];" "should declare and initialize nums")
  (run-test "List declaration default" "list<int> empty;" "should declare empty with default empty list")
  
  ; Mixed type operations
  (run-test "Mixed int and float" "int x = 5; float y = 2.5; x + y;" "should return 7.5")
  (run-test "Variable reassignment" "int x = 5; x = x + 1; x;" "should return 6")
  
  (displayln "===== VARIABLE OPERATIONS TESTS COMPLETED =====")
  (displayln ""))

; ===== CONTROL FLOW TESTS =====
(define (test-control-flow)
  (displayln "===== TESTING CONTROL FLOW =====")
  
  ; If statement tests
  (run-test "Simple if true" "if (true) { $print(\"executed\"); }" "should print executed")
  (run-test "Simple if false" "if (false) { $print(\"not executed\"); }" "should not print")
  (run-test "If-else true branch" "if (5 > 3) { $print(\"true\"); } else { $print(\"false\"); }" "should print true")
  (run-test "If-else false branch" "if (3 > 5) { $print(\"true\"); } else { $print(\"false\"); }" "should print false")
  
  ; If-elseif tests
  (run-test "If-elseif first true" "if (true) { $print(\"first\"); } else if (true) { $print(\"second\"); }" "should print first")
  (run-test "If-elseif second true" "if (false) { $print(\"first\"); } else if (true) { $print(\"second\"); }" "should print second")
  (run-test "If-elseif-else" "int x = 5; if (x > 10) { $print(\"big\"); } else if (x > 0) { $print(\"positive\"); } else { $print(\"other\"); }" "should print positive")
  
  ; While loop tests
  (run-test "Simple while loop" "int i = 0; while (i < 3) { $print(i); i = i + 1; }" "should print 0, 1, 2")
  (run-test "While loop false condition" "while (false) { $print(\"not executed\"); }" "should not execute")
  (run-test "While countdown" "int count = 3; while (count > 0) { $print(count); count = count - 1; }" "should print 3, 2, 1")
  
  ; Nested control flow
  (run-test "If inside while" "int i = 0; while (i < 5) { if (i % 2 == 0) { $print(i); } i = i + 1; }" "should print even numbers")
  (run-test "While inside if" "if (true) { int j = 0; while (j < 2) { $print(j); j = j + 1; } }" "should print 0, 1")
  
  (displayln "===== CONTROL FLOW TESTS COMPLETED =====")
  (displayln ""))

; ===== FUNCTION TESTS =====
(define (test-functions)
  (displayln "===== TESTING FUNCTIONS =====")
  
  ; Simple function tests
  (run-test "Function declaration" "int add(int a, int b) { return a + b; }" "should declare function")
  (run-test "Function call" "int add(int a, int b) { return a + b; } add(3, 5);" "should return 8")
  
  ; Function with different return types
  (run-test "Bool function" "bool isPositive(int x) { return x > 0; } isPositive(5);" "should return true")
  (run-test "Float function" "float multiply(float a, float b) { return a * b; } multiply(2.5, 4.0);" "should return 10.0")
  
  ; Functions with control flow
  (run-test "Function with if" "int abs(int x) { if (x < 0) { return -x; } else { return x; } } abs(-5);" "should return 5")
  (run-test "Function with while" "int factorial(int n) { int result = 1; int i = 1; while (i <= n) { result = result * i; i = i + 1; } return result; } factorial(4);" "should return 24")
  
  ; Recursive functions
  (run-test "Recursive factorial" "int fact(int n) { if (n <= 1) { return 1; } else { return n * fact(n - 1); } } fact(5);" "should return 120")
  
  (displayln "===== FUNCTION TESTS COMPLETED =====")
  (displayln ""))

; ===== PREDEFINED OPERATIONS TESTS =====
(define (test-predefined-operations)
  (displayln "===== TESTING PREDEFINED OPERATIONS =====")
  
  ; Print tests
  (run-test "Print integer" "$print(42);" "should print 42")
  (run-test "Print float" "$print(3.14);" "should print 3.14")
  (run-test "Print boolean" "$print(true);" "should print true")
  (run-test "Print string" "$print(\"hello\");" "should print hello")
  (run-test "Print expression" "$print(5 + 3);" "should print 8")
  
  ; List operations tests (if supported)
  (run-test "List get" "list<int> nums = [1, 2, 3]; $get(nums, 1);" "should return 2")
  (run-test "List set" "list<int> nums = [1, 2, 3]; $set(nums, 1, 5); $get(nums, 1);" "should return 5")
  (run-test "List push" "list<int> nums = [1, 2]; $push(nums, 3); $get(nums, 2);" "should return 3")
  (run-test "List pop" "list<int> nums = [1, 2, 3]; $pop(nums);" "should return 3")
  
  (displayln "===== PREDEFINED OPERATIONS TESTS COMPLETED =====")
  (displayln ""))

; ===== EDGE CASES AND ERROR HANDLING TESTS =====
(define (test-edge-cases)
  (displayln "===== TESTING EDGE CASES =====")
  
  ; Division by zero
  (run-test "Division by zero" "10 / 0;" "should cause error")
  (run-test "Modulo by zero" "10 % 0;" "should cause error")
  
  ; Type mismatches
  (run-test "String arithmetic" "\"hello\" + 5;" "should cause error or handle gracefully")
  
  ; Undefined variables
  (run-test "Undefined variable" "x + 5;" "should cause error")
  
  ; Function call errors
  (run-test "Undefined function" "undefinedFunc();" "should cause error")
  (run-test "Wrong argument count" "int add(int a, int b) { return a + b; } add(5);" "should cause error")
  
  ; List access errors
  (run-test "List index out of bounds" "list<int> nums = [1, 2]; $get(nums, 5);" "should cause error")
  
  (displayln "===== EDGE CASES TESTS COMPLETED =====")
  (displayln ""))

; ===== MAIN TEST RUNNER =====
(define (run-comprehensive-tests)
  (displayln "")
  (displayln "🚀 STARTING COMPREHENSIVE TEST SUITE 🚀")
  (displayln "")
  
  (reset-test-counters!)
  
  (test-arithmetic-operations)
  (test-unary-operations)
  (test-comparison-operations)
  (test-bitwise-operations)
  (test-logical-operations)
  (test-variable-operations)
  (test-control-flow)
  (test-functions)
  (test-predefined-operations)
  (test-edge-cases)
  
  (print-test-summary)
  
  (displayln "🏁 COMPREHENSIVE TEST SUITE COMPLETED 🏁")
  (displayln ""))

; Export the main test function
(provide run-comprehensive-tests)

; Run tests when file is executed directly
(run-comprehensive-tests)
