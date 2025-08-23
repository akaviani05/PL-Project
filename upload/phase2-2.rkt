#lang racket

(require "interpreter.rkt")
(require "lexer.rkt")
(require "parser.rkt")
(require "datatypes.rkt")

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
        (with-handlers 
          ([exn:fail? (lambda (e) (displayln (format "→ Raw: ~s" result)))])
          (cond
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

(run-demo "odd even:"
          "
          $print(\"input n\");
          int n;
          $input(n);

          list A;
          list B;
          int ind = 0;
          while (ind < n) {
              int x;
              $print(\"input one of the base10 numbers\");
              $input(x);
              $push(A, x);
              ind = ind + 1;
          }

          $print(\"input m\");
          int m;
          $input(m);
          
          int odds = 1;
          int evens = 1;
          ind = 0;
          while (ind < m) {
              int x;
              $print(\"input one of the base2 numbers\");
              $input(x);

              int ans = 0;
              int base = 1;
              while (x > 0) {
                  ans = ans + (x % 10) * base;
                  x = x / 10;
                  base = base * 2;
              }

              bool flag = false;
              int i = 0;
              while (i < n) {
                  if ($get(A, i) == ans) {
                      flag = true;
                  }
                  i = i + 1;
              }

              if (flag) {
                  if (ans % 2 == 1) { odds = odds * ans; }
                  else {evens = evens * ans; }
              }

              ind = ind + 1;
          }
          
          $print(odds + evens);
           ")
