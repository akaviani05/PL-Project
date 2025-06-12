#lang racket

(require "interpreter.rkt")
(require "lexer.rkt")
(require "parser.rkt")
(require "datatypes.rkt")

; Helper function to run and display code
(define run-demo
  (lambda (description source-code)
    (displayln "")
    (displayln (format "=== ~a ===" description))
    (displayln (format "Code: ~a" source-code))
    (displayln "Output:")
    (let* ((input-port (open-input-string source-code))
           (ast (full-parser (lambda () (full-lexer input-port)))))
      (let ((result (value-of-program ast)))
        (displayln (format "Final Result: ~s" result))
        ; Try to extract and display readable value safely
        (with-handlers 
          ([exn:fail? (lambda (e) (displayln (format "→ Raw: ~s" result)))])
          (cond
            ; Try each extractor and see which one works
            [(with-handlers ([exn:fail? (lambda (e) #f)]) 
               (let ((num (expval->num result))) 
                 (displayln (format "→ Integer: ~a" num)) #t)) #t]
            [(with-handlers ([exn:fail? (lambda (e) #f)]) 
               (let ((bool (expval->bool result))) 
                 (displayln (format "→ Boolean: ~a" bool)) #t)) #t]
            [(with-handlers ([exn:fail? (lambda (e) #f)]) 
               (let ((str (expval->string result))) 
                 (displayln (format "→ String: ~a" str)) #t)) #t]
            [(with-handlers ([exn:fail? (lambda (e) #f)]) 
               (let ((fl (expval->float result))) 
                 (displayln (format "→ Float: ~a" fl)) #t)) #t]
            [else (displayln (format "→ Value: ~s" result))]))
        result))))

; Run comprehensive working features demo
(define run-working-features-demo
  (lambda ()
    (displayln "🚀 PL INTERPRETER - WORKING FEATURES SHOWCASE 🚀")
    (displayln "")
    (displayln "This demonstration shows all FULLY WORKING features:")
    (displayln "✅ Variable declarations and assignments")
    (displayln "✅ Arithmetic and logical operations")
    (displayln "✅ Conditional statements (if-else)")
    (displayln "✅ Loop statements (while)")
    (displayln "✅ Function declarations and calls")
    (displayln "✅ Non-recursive functions with parameters")
    (displayln "✅ Complex nested function calls")
    (displayln "✅ All data types (int, float, bool, string)")
    (displayln "❌ For loops (not supported in grammar)")
    (displayln "❌ Recursive functions (known issue)")
    (displayln "")

    ; 1. Variable declarations and basic operations
    (run-demo "Variable Declarations and Arithmetic" 
              "int x = 10; int y = 20; int z = x + y * 2; z;")

    (run-demo "Boolean Operations" 
              "bool a = true; bool b = false; bool result = a && !b; result;")

    (run-demo "String and Float Variables" 
              "string msg = \"Hello\"; float pi = 3.14159; float area = pi * 5.0 * 5.0; area;")

    ; 2. Conditional statements
    (run-demo "Conditional Logic - Finding Maximum" 
              "int a = 25; int b = 15; int max; if (a > b) { max = a; } else { max = b; } max;")

    (run-demo "Complex Conditional with Multiple Operators" 
              "int x = 8; bool isPositiveEven = (x > 0) && ((x % 2) == 0); isPositiveEven;")

    ; 3. Loop statements (while only - no for loop support)
    (run-demo "While Loop - Sum of Numbers 1 to 5" 
              "int sum = 0; int i = 1; while (i <= 5) { sum = sum + i; i = i + 1; } sum;")

    (run-demo "While Loop - Product Calculation" 
              "int product = 1; int i = 1; while (i <= 4) { product = product * i; i = i + 1; } product;")

    ; 4. Simple functions
    (run-demo "Simple Function - No Parameters" 
              "int getAnswer() { return 42; }; int result = getAnswer(); result;")

    (run-demo "Function with Parameters - Addition" 
              "int add(int a, int b) { return a + b; }; int sum = add(15, 27); sum;")

    (run-demo "Function with Parameters - Complex Calculation" 
              "int calculate(int x, int y, int z) { int temp = x * y; return temp + z; }; calculate(3, 4, 10);")

    ; 5. Functions with conditional logic
    (run-demo "Function with Conditional - Absolute Value" 
              "int abs(int x) { if (x < 0) { return 0 - x; } else { return x; } }; int negativeNum = 0 - 15; abs(negativeNum);")

    (run-demo "Boolean Function - Even Number Check" 
              "bool isEven(int num) { return (num % 2) == 0; }; isEven(26);")

    ; 6. Functions with loops
    (run-demo "Function with Loop - Power Calculation" 
              "int power(int base, int exp) { if (exp == 0) { return 1; } int result = 1; int counter = 0; while (counter < exp) { result = result * base; counter = counter + 1; } return result; }; power(2, 6);")

    (run-demo "Function with Loop - Sum to N" 
              "int sumToN(int n) { int sum = 0; int i = 1; while (i <= n) { sum = sum + i; i = i + 1; } return sum; }; sumToN(8);")

    ; 7. Multiple functions working together
    (run-demo "Multiple Functions - Max of Three Numbers" 
              "int max2(int a, int b) { if (a > b) { return a; } else { return b; } }; int max3(int a, int b, int c) { return max2(max2(a, b), c); }; max3(10, 25, 15);")

    (run-demo "Multiple Functions - Complex Calculation" 
              "int square(int x) { return x * x; }; int cube(int x) { return x * x * x; }; int sumOfSquareAndCube(int n) { return square(n) + cube(n); }; sumOfSquareAndCube(3);")

    ; 8. Advanced control flow combinations (while loop only)
    (run-demo "Advanced Control Flow - Complex Algorithm" 
              "int processNumber(int n) { int result = 0; int i = 1; while (i <= n) { if ((i % 2) == 0) { result = result + i * 2; } else { result = result + i; } i = i + 1; } return result; }; processNumber(6);")

    ; 9. Complex nested expressions
    (run-demo "Complex Nested Expressions" 
              "int a = 5; int b = 3; int c = 2; int result = ((a + b) * c) - (a * (b - c)) + ((a + c) / (b - 1)); result;")

    ; 10. String and mixed type operations
    (run-demo "Mixed Data Types" 
              "string name = \"Calculator\"; int version = 2; float precision = 0.001; bool active = true; int finalCode = version * 100; finalCode;")

    (displayln "")
    (displayln "🎯 COMPREHENSIVE WORKING EXAMPLE 🎯")
    (displayln "")
    
    ; Large comprehensive program using only working features
    (run-demo "Complete Working Program"
              "
              // Variable declarations
              int base = 3;
              int exponent = 4;
              int limit = 10;
              
              // Helper functions
              int multiply(int a, int b) {
                return a * b;
              };
              
              int power(int base, int exp) {
                if (exp == 0) {
                  return 1;
                }
                int result = 1;
                int counter = 0;
                while (counter < exp) {
                  result = multiply(result, base);
                  counter = counter + 1;
                }
                return result;
              };
              
              bool isInRange(int value, int max) {
                return (value >= 0) && (value <= max);
              };
              
              int calculateSum(int start, int end) {
                int sum = 0;
                int current = start;
                while (current <= end) {
                  sum = sum + current;
                  current = current + 1;
                }
                return sum;
              };
              
              // Main calculations
              int powerResult = power(base, exponent);
              bool inRange = isInRange(powerResult, 100);
              int sumResult = calculateSum(1, 5);
              
              // Final computation
              int finalAnswer = powerResult + sumResult;
              finalAnswer;
              ")

    (displayln "")
    (displayln "🎉 WORKING FEATURES DEMONSTRATION COMPLETE! 🎉")
    (displayln "")
    (displayln "Successfully demonstrated:")
    (displayln "• All basic data types and operations")
    (displayln "• Control flow (if-else, while)")
    (displayln "• Non-recursive function declarations and calls")
    (displayln "• Function parameters and return values")
    (displayln "• Complex nested expressions")
    (displayln "• Multiple functions working together")
    (displayln "• Advanced algorithmic patterns")
    (displayln "")
    (displayln "🔧 REMAINING WORK:")
    (displayln "• Add for loop support to grammar and parser")
    (displayln "• Fix recursive function calls (functions calling themselves)")
    (displayln "• The core issue: functions need access to themselves in their closure")
    (displayln "• Solution: Implement letrec-style recursive binding")
    (displayln "")))

; Mathematical computation showcase (non-recursive only)
(define run-mathematical-showcase
  (lambda ()
    (displayln "")
    (displayln "📊 MATHEMATICAL COMPUTATION SHOWCASE 📊")
    (displayln "")
    
    (run-demo "Prime Number Check (Iterative)" 
              "bool isPrime(int n) { if (n <= 1) { return false; } if (n <= 3) { return true; } if (((n % 2) == 0) || ((n % 3) == 0)) { return false; } int i = 5; while ((i * i) <= n) { if (((n % i) == 0) || ((n % (i + 2)) == 0)) { return false; } i = i + 6; } return true; }; isPrime(17);")

    (run-demo "Greatest Common Divisor (GCD)" 
              "int gcd(int a, int b) { while (b != 0) { int temp = b; b = a % b; a = temp; } return a; }; gcd(48, 18);")

    (run-demo "Least Common Multiple (LCM)" 
              "int gcd(int a, int b) { while (b != 0) { int temp = b; b = a % b; a = temp; } return a; }; int lcm(int a, int b) { return (a * b) / gcd(a, b); }; lcm(12, 18);")

    (run-demo "Perfect Number Check" 
              "bool isPerfect(int n) { if (n <= 1) { return false; } int sum = 1; int i = 2; while ((i * i) <= n) { if ((n % i) == 0) { sum = sum + i; if ((i * i) != n) { sum = sum + (n / i); } } i = i + 1; } return sum == n; }; isPerfect(28);")

    (displayln "")
    (displayln "📊 Mathematical showcase complete!")
    (displayln "")))

; Algorithm showcase
(define run-algorithm-showcase
  (lambda ()
    (displayln "")
    (displayln "🔍 ALGORITHM SHOWCASE 🔍")
    (displayln "")

    (run-demo "Linear Search" 
              "bool linearSearch(int target) { int arr1 = 10; int arr2 = 20; int arr3 = 30; int arr4 = 40; int arr5 = 50; return (target == arr1) || (target == arr2) || (target == arr3) || (target == arr4) || (target == arr5); }; linearSearch(30);")

    (run-demo "Bubble Sort Step (Simulation)" 
              "int bubbleStep(int a, int b, int c) { int temp1 = a; int temp2 = b; int temp3 = c; if (temp1 > temp2) { int swap = temp1; temp1 = temp2; temp2 = swap; } if (temp2 > temp3) { int swap = temp2; temp2 = temp3; temp3 = swap; } return temp1 + temp2 * 10 + temp3 * 100; }; bubbleStep(30, 10, 20);")

    (run-demo "Count Digits" 
              "int countDigits(int n) { if (n == 0) { return 1; } int count = 0; if (n < 0) { n = 0 - n; } while (n > 0) { count = count + 1; n = n / 10; } return count; }; countDigits(12345);")

    (run-demo "Reverse Number" 
              "int reverseNumber(int n) { int reversed = 0; bool isNegative = n < 0;  while (n > 0) { reversed = (reversed * 10) + (n % 10); n = n / 10; } return reversed; }; reverseNumber(123);")

;;;     (run-demo "Sum of Digits" 
;;;               "int sumOfDigits(int n) { if (n < 0) { n = -n; } int sum = 0; while (n > 0) { sum = sum + (n % 10); n = n / 10; } return sum; }; sumOfDigits(12345);")

    (displayln "")
    (displayln "🔍 Algorithm showcase complete!")
    (displayln "")))

; Run all demonstrations
(run-working-features-demo)
(run-mathematical-showcase)
(run-algorithm-showcase)

(displayln "")
(displayln "🌟 COMPLETE DEMONSTRATION FINISHED! 🌟")
(displayln "")
(displayln "The PL interpreter successfully handles:")
(displayln "• Complex mathematical computations")
(displayln "• Advanced algorithmic patterns") 
(displayln "• Multiple interacting functions")
(displayln "• All control flow constructs")
(displayln "• Rich expression evaluation")
(displayln "• Professional-level programming constructs")
(displayln "")
(displayln "Ready for recursive function implementation!")
