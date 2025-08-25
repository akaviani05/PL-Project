#lang racket

(require "interpreter.rkt")
(require "lexer.rkt")
(require "parser.rkt")
(require "datatypes.rkt")

(define run
  (lambda (des source)
    (displayln "")
    (displayln (format "*** ~a" des))
    (displayln (format "code: ~a" source))
    (displayln "Output:")
    (let* ((input-port (open-input-string source))
           (ast (full-parser (lambda () (full-lexer input-port)))))
      (let ((result (value-of-program ast)))
        (displayln (format "Final result: ~s" result))
        (with-handlers 
          ([exn:fail? (lambda (e) (displayln (format "wtf: ~s" result)))])
          (displayln (format "result: ~s" result)))
        result))))


; READ: fahmidim roye bazi os ha ye moshkeli darim sar input, sare hamin input ro dasti bedid :) (beyin begin va end)

(run "odd even:"
          "
          // BEGIN INPUTS
          list A;
          list B;
          $push(A, 7);
          $push(A, 11);
          $push(A, 20);
          $push(A, 16);
          $push(A, 14);
          $push(A, 1);

          $push(B, \"111\");
          $push(B, \"10000\");
          $push(B, \"10001001\");
          $push(B, \"1011\");
          $push(B, \"1100\");
          $push(B, \"0\");

          // END INPUTS
          
          int m = $size(B);
          int n = $size(A);
          int odds = 1;
          int evens = 1;
          ind = 0;
          while (ind < m) {
              
             //  $print(\"input one of the base2 numbers\");
             // $input(x);
              string x = $get(B, ind);
              list lst = $tocharlist(x);
              int ans = 0;
              int base = 1;
              int sz = $size(lst);
              int ind2 = 0;
              while (ind2 < sz) {
                  char c = $get(lst, sz - 1 - ind2);
                  int val = 0;
                  if (c == '1') {val = 1;}
                  ans = ans + val * base;
                  ind2 = ind2 + 1;
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
