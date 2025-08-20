#lang racket

(require "interpreter.rkt")
(require "lexer.rkt")
(require "parser.rkt")


(define test-parse-and-interpret
  (lambda (source-code)
    (displayln (format "Testing: ~s" source-code))
    (let* ((input-port (open-input-string source-code))
           (ast (full-parser (lambda () (full-lexer input-port)))))
      
      (let ((result (value-of-program ast)))
        (displayln (format "Result: ~s" result))
        result))))


(define run-basic-tests
  (lambda ()
    (displayln "************ -> basic tests")
    

    (test-parse-and-interpret "5 + 3;")
    (displayln "")
    

    (test-parse-and-interpret "10 - 4;")
    (displayln "")
    

    (test-parse-and-interpret "6 * 7;")
    (displayln "")
    

    (test-parse-and-interpret "20 / 4;")
    (displayln "")
    

    (test-parse-and-interpret "17 % 5;")
    (displayln "")
    

    (test-parse-and-interpret "2 + 3 * 4;")
    (displayln "")
    

    (test-parse-and-interpret "(2 + 3) * 4;")
    (displayln "")
    

    (test-parse-and-interpret "15 + 7 - 3 * 2;")
    (displayln "")))


(define run-unary-tests
  (lambda ()
    (displayln "************ -> unary ops")
    

    (test-parse-and-interpret "~5;")
    (displayln "")
    

    (test-parse-and-interpret "!true;")
    (displayln "")
    

    (test-parse-and-interpret "!false;")
    (displayln "")))


(define run-comparison-tests
  (lambda ()
    (displayln "************ -> comparison tests")
    

    (test-parse-and-interpret "5 < 10;")
    (displayln "")
    
    (test-parse-and-interpret "10 < 5;")
    (displayln "")
    

    (test-parse-and-interpret "10 > 5;")
    (displayln "")
    
    (test-parse-and-interpret "5 > 10;")
    (displayln "")
    

    (test-parse-and-interpret "5 <= 10;")
    (displayln "")
    
    (test-parse-and-interpret "10 <= 10;")
    (displayln "")
    
    (test-parse-and-interpret "15 <= 10;")
    (displayln "")
    

    (test-parse-and-interpret "10 >= 5;")
    (displayln "")
    
    (test-parse-and-interpret "10 >= 10;")
    (displayln "")
    
    (test-parse-and-interpret "5 >= 10;")
    (displayln "")
    

    (test-parse-and-interpret "5 == 5;")
    (displayln "")
    
    (test-parse-and-interpret "5 == 10;")
    (displayln "")
    
    (test-parse-and-interpret "true == true;")
    (displayln "")
    
    (test-parse-and-interpret "true == false;")
    (displayln "")
    

    (test-parse-and-interpret "5 != 10;")
    (displayln "")
    
    (test-parse-and-interpret "5 != 5;")
    (displayln "")))


(define run-bitwise-tests
  (lambda ()
    (displayln "************ -> bitwise ops tests")
    

    (test-parse-and-interpret "5 & 3;")  
    (displayln "")
    
    (test-parse-and-interpret "12 & 10;")  
    (displayln "")
    

    (test-parse-and-interpret "5 ^ 3;") 
    (displayln "")
    
    (test-parse-and-interpret "12 ^ 10;") 
    (displayln "")
    

    (test-parse-and-interpret "5 | 3;")
    (displayln "")
    
    (test-parse-and-interpret "12 | 10;")
    (displayln "")))


(define run-logical-tests
  (lambda ()
    (displayln "************ -> logical ops tests")

    (test-parse-and-interpret "true && true;")
    (displayln "")
    
    (test-parse-and-interpret "true && false;")
    (displayln "")
    
    (test-parse-and-interpret "false && true;")
    (displayln "")
    
    (test-parse-and-interpret "false && false;")
    (displayln "")
    

    (test-parse-and-interpret "true || true;")
    (displayln "")
    
    (test-parse-and-interpret "true || false;")
    (displayln "")
    
    (test-parse-and-interpret "false || true;")
    (displayln "")
    
    (test-parse-and-interpret "false || false;")
    (displayln "")))


(define run-complex-tests
  (lambda ()
      (displayln "************ -> multiops tests")
    

    (test-parse-and-interpret "5 + 3 > 7;")
    (displayln "")
    
    (test-parse-and-interpret "10 - 5 == 5;")
    (displayln "")
    

    (test-parse-and-interpret "5 > 3 && 10 < 15;")
    (displayln "")
    
    (test-parse-and-interpret "5 > 10 || 3 < 7;")
    (displayln "")
    

    (test-parse-and-interpret "(5 + 3) * 2 > 15 && true;")
    (displayln "")
    

    (test-parse-and-interpret "(8 + 4) & (10 - 2);")
    (displayln "")))


(define run-edge-case-tests
  (lambda ()
      (displayln "************ -> edgecase tests")
    

    (test-parse-and-interpret "0 + 5;")
    (displayln "")
    
    (test-parse-and-interpret "5 * 0;")
    (displayln "")
    

    (test-parse-and-interpret "10 - 15;")
    (displayln "")
    

    (test-parse-and-interpret "((10 + 5) * 2) / 3;")
    (displayln "")
    

    (test-parse-and-interpret "false && true;")
    (displayln "")
    
    (test-parse-and-interpret "true || false;")
    (displayln "")))


(define run-variable-tests
  (lambda ()
      (displayln "************ -> var decleration tests")
    

    (test-parse-and-interpret "int x = 5;")
    (displayln "")
    

    (test-parse-and-interpret "int y;")
    (displayln "")

    (test-parse-and-interpret "bool b = false;")
    (displayln "")

    (test-parse-and-interpret "string s = \"salam\";")
    (displayln "")

    (test-parse-and-interpret "char c = 'a';")
    (displayln "")

    (test-parse-and-interpret "list emp;")
    (displayln "")

    (test-parse-and-interpret "int getVal(){return 42;};")
    (displayln "")
  ))


(define run-print-tests
  (lambda ()
      (displayln "************ -> $print tests")
    

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
    

    (test-parse-and-interpret "$print(5 + 3);")
    (displayln "")
    
    (test-parse-and-interpret "$print(10 * 2 - 5);")
    (displayln "")
    
    (test-parse-and-interpret "$print(20 / 4);")
    (displayln "")
    
    (test-parse-and-interpret "$print(17 % 5);")
    (displayln "")
    

    (test-parse-and-interpret "$print(5 > 3);")
    (displayln "")
    
    (test-parse-and-interpret "$print(2 == 2);")
    (displayln "")
    
    (test-parse-and-interpret "$print(7 <= 5);")
    (displayln "")
    

    (test-parse-and-interpret "int x = 100; $print(x);")
    (displayln "")
    
    (test-parse-and-interpret "bool flag = true; $print(flag);")
    (displayln "")
    
    (test-parse-and-interpret "string msg = \"test\"; $print(msg);")
    (displayln "")
    

    (test-parse-and-interpret "$print(1); $print(2); $print(3);")
    (displayln "")
    
    (displayln "")))


(define run-if-tests
  (lambda ()
      (displayln "************ -> if tests")
    

    (test-parse-and-interpret "if (5 > 3) { $print(\"condition is true\"); }")
    (displayln "")
    

    (test-parse-and-interpret "if (3 > 5) { $print(\"this should not print\"); }")
    (displayln "")
    

    (test-parse-and-interpret "if (10 == 10) { $print(\"equal\"); } else { $print(\"not equal\"); }")
    (displayln "")
    

    (test-parse-and-interpret "if (10 == 5) { $print(\"equal\"); } else { $print(\"not equal\"); }")
    (displayln "")
    

    (test-parse-and-interpret "if (true) { if (5 < 10) { $print(\"nested condition true\"); } }")
    (displayln "")
    

    (test-parse-and-interpret "int x = 15; if (x > 10) { $print(\"x is greater than 10\"); }")
    (displayln "")
    

    (test-parse-and-interpret "int a = 8; int b = 12; if (a + b > 15) { $print(\"sum is large\"); } else { $print(\"sum is small\"); }")
    (displayln "")
    

    (test-parse-and-interpret "int score = 85; if (score >= 90) { $print(\"A grade\"); } else if (score >= 80) { $print(\"B grade\"); } else { $print(\"C grade\"); }")
    (displayln "")
    

    (test-parse-and-interpret "int num = 0; if (num > 0) { $print(\"positive\"); } else if (num < 0) { $print(\"negative\"); } else { $print(\"zero\"); }")
    (displayln "")
    

    (test-parse-and-interpret "bool flag = true; if (flag) { $print(\"flag is true\"); } else { $print(\"flag is false\"); }")
    (displayln "")
    (displayln "")))


(define run-while-tests
  (lambda ()
      (displayln "************ -> while tests")
    

    (test-parse-and-interpret "int i = 0; while (i < 3) { $print(i); i = i + 1; }")
    (displayln "")
    

    (test-parse-and-interpret "int count = 5; while (count > 0) { $print(count); count = count - 1; }")
    (displayln "")
    

    (test-parse-and-interpret "int x = 10; while (x < 5) { $print(\"this should not print\"); }")
    (displayln "")
    

    (test-parse-and-interpret "bool running = true; int counter = 0; while (running) { $print(counter); counter = counter + 1; if (counter >= 3) { running = false; } }")
    (displayln "")
    

    (test-parse-and-interpret "int outer = 0; while (outer < 2) { int inner = 0; while (inner < 2) { $print(outer * 10 + inner); inner = inner + 1; } outer = outer + 1; }")
    (displayln "")
    

    (test-parse-and-interpret "int a = 1; int b = 10; while (a < b && a < 5) { $print(a); a = a + 1; }")
    (displayln "")
    

    (test-parse-and-interpret "int sum = 0; int n = 1; while (n <= 4) { sum = sum + n; n = n + 1; } $print(sum);")
    (displayln "")
    
    (displayln "")))


(define run-control-flow-tests
  (lambda ()
      (displayln "************ -> control flow tests")

    (test-parse-and-interpret "int i = 0; while (i < 5) { if (i % 2 == 0) { $print(\"even: \"); $print(i); } i = i + 1; }")
    (displayln "")
    

    (test-parse-and-interpret "bool condition = true; if (condition) { int j = 0; while (j < 3) { $print(j); j = j + 1; } }")
    (displayln "")
    

    (test-parse-and-interpret "int x = 8; if (x > 5) { int k = 0; while (k < x - 5) { $print(k); k = k + 1; } } else { $print(\"x is too small\"); }")
    (displayln "")
    

    (test-parse-and-interpret "int limit = 3; int i = 0; while (i < limit) { if (i == 1) { $print(\"middle\"); } else if (i == 0) { $print(\"start\"); } else { $print(\"end\"); } i = i + 1; }")
    (displayln "")
    
    (displayln "")))


(define run-size-tests
  (lambda ()
      (displayln "************ -> $size tests")
    

    (test-parse-and-interpret "list empty; $print($size(empty));")
    (displayln "")
    

    (test-parse-and-interpret "list numbers; $push(numbers, 1); $push(numbers, 2); $print($size(numbers));")
    (displayln "")
    
    (displayln "")))


(define run-tocharlist-tests
  (lambda ()
    (displayln "************ -> $tochars tests")
    

    (test-parse-and-interpret "string s = \"abc\"; list chars = $tocharlist(s); $print(chars);")
    (displayln "")
    

    (test-parse-and-interpret "string empty = \"\"; list emptyChars = $tocharlist(empty); $print(emptyChars);")
    (displayln "")
    

    (test-parse-and-interpret "string single = \"x\"; list singleChar = $tocharlist(single); $print(singleChar);")
    (displayln "")
    

    (test-parse-and-interpret "string message = \"Hello World\"; list messageChars = $tocharlist(message); $print(messageChars);")
    (displayln "")
    

    (test-parse-and-interpret "string special = \"123!@#\"; list specialChars = $tocharlist(special); $print(specialChars);")
    (displayln "")
    

    (test-parse-and-interpret "$print($tocharlist(\"test\"));")
    (displayln "")
    

    (test-parse-and-interpret "string text = \"racket\"; $print($tocharlist(text));")
    (displayln "")
    

    (test-parse-and-interpret "string word = \"hi\"; list letters = $tocharlist(word); $print($get(letters, 0)); $print($get(letters, 1));")
    (displayln "")
    

    (test-parse-and-interpret "string original = \"abc\"; list chars = $tocharlist(original); $set(chars, 1, 'X'); $print(chars);")
    (displayln "")
    

    (test-parse-and-interpret "string base = \"ab\"; list chars = $tocharlist(base); $push(chars, 'c'); $print(chars);")
    (displayln "")
    
    (displayln "")))

  (define run-function-tests
  (lambda ()
    (displayln "************ -> function tests")
    

    (test-parse-and-interpret "int getValue() {return 42;}; getValue();")
    (displayln "")
    
    (test-parse-and-interpret "int add(int a, int b) {return a + b;}; add(10, 22);")
    (displayln "")

    (test-parse-and-interpret "int getValue() { return 10; }; int double(int x) { return x * 2; }; double(getValue());")
    (displayln "")

    (test-parse-and-interpret "int fact(int n) {if (n == 0) {return 1;} else {return fact(n - 1) * n;}}; fact(6);")
    (displayln "")

    (displayln "")))

(define run-list-tests
(lambda ()
      (displayln "************ -> list tests")

      (test-parse-and-interpret "list emptylist;")
      (displayln "")

      (test-parse-and-interpret "list list123; $push(list123, 1); $push(list123, 2); $push(list123, 3); list123;")
      (displayln "")

      (test-parse-and-interpret "list listpop; $push(listpop, 1); $push(listpop, 2); $push(listpop, 3); $pop(listpop); listpop;")
      (displayln "")

      (test-parse-and-interpret "list list123; $push(list123, 1); $push(list123, 2); $push(list123, 3); $get(list123, 1);")
      (displayln "")

      (test-parse-and-interpret "list list173; $push(list173, 1); $push(list173, 2); $push(list173, 3); $set(list173, 1, 7); list173;")
      (displayln "")

      (displayln "")))

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
    (run-function-tests)
    (run-list-tests)
    (run-size-tests)       
    (run-tocharlist-tests)
    (displayln "Yayyyyyy")))

(run-all-tests)
(provide run-all-tests run-basic-tests run-unary-tests run-comparison-tests 
         run-bitwise-tests run-logical-tests run-complex-tests 
         run-edge-case-tests run-variable-tests run-print-tests 
         run-if-tests run-while-tests run-control-flow-tests run-size-tests run-tocharlist-tests test-parse-and-interpret)
