#lang racket

(require "interpreter.rkt")
(require "lexer.rkt")
(require "parser.rkt")

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
        result))))

; Complex program demonstrating all features
(define comprehensive-program "
  // Variable declarations and basic arithmetic
  int x = 10;
  int y = 5;
  float pi = 3.14;
  bool isValid = true;
  string message = \"Hello World\";
  
  // Function to calculate factorial
  int factorial(int n) {
    if (n <= 1) {
      return 1;
    } else {
      return n * factorial(n - 1);
    }
  }
  
  // Function to check if number is even
  bool isEven(int num) {
    return (num % 2) == 0;
  }
  
  // Function to find maximum of two numbers
  int max(int a, int b) {
    if (a > b) {
      return a;
    } else {
      return b;
    }
  }
  
  // Function to calculate sum of numbers from 1 to n
  int sumToN(int n) {
    int sum = 0;
    int i = 1;
    while (i <= n) {
      sum = sum + i;
      i = i + 1;
    }
    return sum;
  }
  
  // Function to calculate power (base^exp)
  int power(int base, int exp) {
    if (exp == 0) {
      return 1;
    }
    int result = 1;
    int counter = 0;
    while (counter < exp) {
      result = result * base;
      counter = counter + 1;
    }
    return result;
  }
  
  // Function to calculate fibonacci number
  int fibonacci(int n) {
    if (n <= 1) {
      return n;
    } else {
      return fibonacci(n - 1) + fibonacci(n - 2);
    }
  }
  
  // Function with complex logic
  int complexCalculation(int a, int b, int c) {
    int temp1 = a + b;
    int temp2 = b * c;
    int temp3 = max(temp1, temp2);
    
    if (isEven(temp3)) {
      temp3 = temp3 + 10;
    } else {
      temp3 = temp3 - 5;
    }
    
    return temp3;
  }
  
  // Main computation
  int fact5 = factorial(5);
  int sum10 = sumToN(10);
  int pow23 = power(2, 3);
  int fib7 = fibonacci(7);
  bool evenCheck = isEven(fact5);
  int maxResult = max(fact5, sum10);
  int complexResult = complexCalculation(10, 20, 3);
  
  // Final calculation combining everything
  int finalResult = (fact5 + sum10 + pow23 + fib7 + maxResult + complexResult) / 6;
  
  finalResult;
")

; Run the comprehensive demo
(define run-comprehensive-demo
  (lambda ()
    (displayln "🚀 COMPREHENSIVE PL INTERPRETER DEMONSTRATION 🚀")
    (displayln "")
    (displayln "This program demonstrates:")
    (displayln "• Variable declarations (int, float, bool, string)")
    (displayln "• Function declarations with parameters and return values")
    (displayln "• Recursive functions (factorial, fibonacci)")
    (displayln "• Iterative functions (sumToN, power)")
    (displayln "• Conditional statements (if-else)")
    (displayln "• Loop statements (while)")
    (displayln "• Arithmetic operations (+, -, *, /, %)")
    (displayln "• Comparison operations (==, !=, <, >, <=, >=)")
    (displayln "• Logical operations (&&, ||, !)")
    (displayln "• Function calls with arguments")
    (displayln "• Nested function calls")
    (displayln "• Complex expressions and calculations")
    (displayln "")
    
    ; Run smaller components first to show progression
    (run-demo "Basic Variable Declaration" 
              "int x = 42; x;")
    
    (run-demo "Simple Function Declaration and Call" 
              "int getValue() { return 100; }; getValue();")
    
    (run-demo "Function with Parameters" 
              "int add(int a, int b) { return a + b; }; add(15, 25);")
    
    (run-demo "Recursive Function - Factorial" 
              "int factorial(int n) { if (n <= 1) { return 1; } else { return n * factorial(n - 1); } }; factorial(5);")
    
    (run-demo "Iterative Function - Sum to N" 
              "int sumToN(int n) { int sum = 0; int i = 1; while (i <= n) { sum = sum + i; i = i + 1; } return sum; }; sumToN(10);")
    
    (run-demo "Conditional Logic - Max Function" 
              "int max(int a, int b) { if (a > b) { return a; } else { return b; } }; max(25, 15);")
    
    (run-demo "Boolean Function - Even Check" 
              "bool isEven(int num) { return (num % 2) == 0; }; isEven(120);")
    
    (run-demo "Power Function with While Loop" 
              "int power(int base, int exp) { if (exp == 0) { return 1; } int result = 1; int counter = 0; while (counter < exp) { result = result * base; counter = counter + 1; } return result; }; power(2, 8);")
    
    (run-demo "Recursive Fibonacci" 
              "int fibonacci(int n) { if (n <= 1) { return n; } else { return fibonacci(n - 1) + fibonacci(n - 2); } }; fibonacci(8);")
    
    (run-demo "Complex Nested Function Calls" 
              "int factorial(int n) { if (n <= 1) { return 1; } else { return n * factorial(n - 1); } }; int max(int a, int b) { if (a > b) { return a; } else { return b; } }; int sumToN(int n) { int sum = 0; int i = 1; while (i <= n) { sum = sum + i; i = i + 1; } return sum; }; max(factorial(5), sumToN(10));")
    
    ; Now run the full comprehensive program
    (displayln "")
    (displayln "🎯 RUNNING FULL COMPREHENSIVE PROGRAM 🎯")
    (displayln "")
    (displayln "This is a complex program with multiple functions, variables, and computations:")
    (run-demo "Complete Comprehensive Program" comprehensive-program)
    
    (displayln "")
    (displayln "🎉 DEMONSTRATION COMPLETE! 🎉")
    (displayln "")
    (displayln "The PL interpreter successfully executed:")
    (displayln "• 8+ function declarations")
    (displayln "• 10+ variable declarations") 
    (displayln "• Multiple recursive calls")
    (displayln "• Multiple iterative loops")
    (displayln "• Complex nested expressions")
    (displayln "• All major language constructs")
    (displayln "")))

; Advanced mathematical computation demo
(define mathematical-demo "
  // Advanced mathematical computations
  
  // Function to calculate greatest common divisor (GCD)
  int gcd(int a, int b) {
    while (b != 0) {
      int temp = b;
      b = a % b;
      a = temp;
    }
    return a;
  }
  
  // Function to calculate least common multiple (LCM)
  int lcm(int a, int b) {
    return (a * b) / gcd(a, b);
  }
  
  // Function to check if a number is prime
  bool isPrime(int n) {
    if (n <= 1) {
      return false;
    }
    if (n <= 3) {
      return true;
    }
    if ((n % 2) == 0) {
      return false;
    }
    
    int i = 3;
    while ((i * i) <= n) {
      if ((n % i) == 0) {
        return false;
      }
      i = i + 2;
    }
    return true;
  }
  
  // Function to find nth prime number
  int nthPrime(int n) {
    if (n == 1) {
      return 2;
    }
    
    int count = 1;
    int num = 3;
    
    while (count < n) {
      if (isPrime(num)) {
        count = count + 1;
      }
      if (count < n) {
        num = num + 2;
      }
    }
    return num;
  }
  
  // Calculate some mathematical results
  int gcd_result = gcd(48, 18);
  int lcm_result = lcm(12, 15);
  bool prime_check = isPrime(17);
  int fifth_prime = nthPrime(5);
  
  // Complex calculation combining all results
  int final_math_result = gcd_result + lcm_result + fifth_prime;
  
  final_math_result;
")

; Algorithm demonstration
(define algorithm-demo "
  // Algorithm implementations
  
  // Binary search (simplified for integers 1 to n)
  bool binarySearch(int target, int n) {
    int left = 1;
    int right = n;
    
    while (left <= right) {
      int mid = (left + right) / 2;
      if (mid == target) {
        return true;
      } else {
        if (mid < target) {
          left = mid + 1;
        } else {
          right = mid - 1;
        }
      }
    }
    return false;
  }
  
  // Calculate sum of digits
  int sumOfDigits(int n) {
    int sum = 0;
    while (n > 0) {
      sum = sum + (n % 10);
      n = n / 10;
    }
    return sum;
  }
  
  // Reverse a number
  int reverseNumber(int n) {
    int reversed = 0;
    while (n > 0) {
      reversed = (reversed * 10) + (n % 10);
      n = n / 10;
    }
    return reversed;
  }
  
  // Check if number is palindrome
  bool isPalindrome(int n) {
    return n == reverseNumber(n);
  }
  
  // Test the algorithms
  bool search_result = binarySearch(7, 10);
  int digit_sum = sumOfDigits(12345);
  int reversed = reverseNumber(12321);
  bool palindrome_check = isPalindrome(12321);
  
  // Combine results
  int algorithm_result = digit_sum + reversed;
  
  algorithm_result;
")

; Run additional demonstrations
(define run-additional-demos
  (lambda ()
    (displayln "")
    (displayln "🔢 MATHEMATICAL ALGORITHMS DEMONSTRATION 🔢")
    (run-demo "Advanced Mathematical Computations" mathematical-demo)
    
    (displayln "")
    (displayln "🧮 ALGORITHM IMPLEMENTATIONS DEMONSTRATION 🧮") 
    (run-demo "Algorithm Implementations" algorithm-demo)
    
    (displayln "")
    (displayln "🏆 ALL DEMONSTRATIONS COMPLETED SUCCESSFULLY! 🏆")))

; Main execution
(displayln "Starting comprehensive PL interpreter demonstration...")
(run-comprehensive-demo)
(run-additional-demos)
