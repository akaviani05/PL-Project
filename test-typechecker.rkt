#lang racket

(require "lexer.rkt"
         "parser.rkt"
         "typechecker.rkt")

(define (test-parse-and-typecheck source)
  (displayln (format "=== Testing: ~a" source))
  (with-handlers
      ([exn:fail? (lambda (e)
                    (displayln (format "Error: ~a" (exn-message e)))
                    (void))])
    (let* ((input-port (open-input-string source))
           (ast (full-parser (lambda () (full-lexer input-port))))
           (res (typecheck-program ast)))
      (displayln (format "Typecheck result: ~s" res))
      res)))

(define run-typechecker-tests
  (lambda ()
    (displayln "=== Running extended typechecker tests ===")

    ;; Basic declarations and arithmetic
    (test-parse-and-typecheck "int x = 10; string s = \"hi\"; int y = x + 5;")
    (test-parse-and-typecheck "float f = 1.5; int i = 2; float r = f + i;") ; promotion
    (test-parse-and-typecheck "int a; float b; bool c; string t; char ch; list L;")

    ;; Type mismatches in declarations/initializers
    (test-parse-and-typecheck "int x = 3.14;")                     ; float->int initializer (should error)
    (test-parse-and-typecheck "string s = 5;")                     ; int->string (should error)
    (test-parse-and-typecheck "bool b = 1;")                       ; int->bool (should error)

    ;; Undefined variables
    (test-parse-and-typecheck "$print(u);")
    (test-parse-and-typecheck "int x = 1; $print(y + x);")         ; y undefined

    ;; Assignments
    (test-parse-and-typecheck "int x = 5; x = x + 1;")             ; ok
    (test-parse-and-typecheck "int x = 5; x = \"hello\";")         ; assignment type mismatch

    ;; Arithmetic and operators
    (test-parse-and-typecheck "int a = 5; int b = 2; int c = a % b;")
    (test-parse-and-typecheck "int a = 5; float b = 2.0; float c = a / b;")
    (test-parse-and-typecheck "int a = 5; string s = \"s\"; int z = a + s;") ; invalid +

    ;; Comparisons and logical ops
    (test-parse-and-typecheck "int x = 1; bool t = x < 3;")
    (test-parse-and-typecheck "string s = \"a\"; bool t = s == \"b\";")
    (test-parse-and-typecheck "int x = 1; bool bad = x && true;") ; && requires bools

    ;; Bitwise requires ints
    (test-parse-and-typecheck "int a = 3; int b = 4; int d = a & b;")
    (test-parse-and-typecheck "float f = 1.0; int x = f & 1;")    ; should error

    ;; If/while conditions must be bool
    (test-parse-and-typecheck "if (true) { int x = 1; }")
    (test-parse-and-typecheck "if (1) { int x = 1; }")             ; non-bool condition
    (test-parse-and-typecheck "while (false) { int x = 0; }")
    (test-parse-and-typecheck "while (0) { int x = 0; }")          ; non-bool condition

    ;; Functions: declarations and calls
    (test-parse-and-typecheck "int id(int x) { return x; }; int r = id(5);")
    (test-parse-and-typecheck "int id(int x) { return x; }; int r = id(true);") ; arg type mismatch
    (test-parse-and-typecheck "int add(int a, int b) { return a + b; }; int r = add(1);") ; arity mismatch
    (test-parse-and-typecheck "int add(int a, int b) { return a + b; }; int r = add(1, 2, 3);") ; arity mismatch

    ;; Recursion (should typecheck)
    (test-parse-and-typecheck "int fact(int n) { if (n == 0) { return 1; } else { return n * fact(n - 1); } };")

    ;; Nested calls and return value usage
    (test-parse-and-typecheck "int add(int a, int b) { return a + b; }; int twice(int x){ return add(x,x); }; int r = twice(3);")

    ;; Return type checking (note: current checker doesn't fully validate return statements in body)
    (test-parse-and-typecheck "int wrong() { return true; };") ; may or may not be caught depending on checker

    ;; Predefined list operations (some checks require list var to exist)
    (test-parse-and-typecheck "list a; $push(a, 1); $push(a, 2); $print($get(a, 0));") ; ok
    (test-parse-and-typecheck "$push(b, 1);") ; pushing to undeclared list (should warn/error if checked)
    (test-parse-and-typecheck "list x; $print($get(x, 0));") ; get on possibly empty but type-wise ok

    ;; Indexing & set/push/pop signatures (typechecker has limited checks)
    (test-parse-and-typecheck "list L; $push(L, 1); $set(L, 0, 5); $pop(L); $size(L);")

    ;; String/char handling
    (test-parse-and-typecheck "string s = \"hello\"; list cs = $tocharlist(s);")
    (test-parse-and-typecheck "char c = 'a'; int bad = c + 1;") ; char + int may be invalid

    ;; Edge cases with literals and values
    (test-parse-and-typecheck "int x = -5 + 10;")
    (test-parse-and-typecheck "float x = -5.5 + 1.2;")
    (test-parse-and-typecheck "bool b = !true;")

    ;; Many small combined tests
    (for ([i (in-range 1 10)])
      (test-parse-and-typecheck (format "int v~a = ~a; int w~a = v~a + 1;" i i i i)))

    (displayln "=== Extended tests done ===")))

(run-typechecker-tests)
