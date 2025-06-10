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

; Test the basic function with semicolon
(displayln "🔧 TESTING FUNCTION DECLARATION WITH SEMICOLON")

; Simple test
(run-demo "Simple Function with Semicolon" 
          "int double(int x) { return x * 2; }; double(5);")

; Test comprehensive program with all semicolons added
(run-demo "Comprehensive Program with Proper Semicolons"
          "
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
          
          int base = 3;
          int exponent = 4;
          int powerResult = power(base, exponent);
          powerResult;
          ")

; Test multiple functions
(run-demo "Multiple Functions with Semicolons"
          "
          int square(int x) { 
            return x * x; 
          };
          
          int cube(int x) { 
            return x * x * x; 
          };
          
          int sumOfSquareAndCube(int n) { 
            return square(n) + cube(n); 
          };
          
          sumOfSquareAndCube(3);
          ")

(displayln "")
(displayln "✅ Function declaration semicolon tests complete!")
