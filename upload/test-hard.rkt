#lang racket
(require "interpreter.rkt")
(require "lexer.rkt")
(require "parser.rkt")

(define test-parse-and-interpret
  (lambda (source-code)
    (displayln (format "Testing: ~s" source-code))
    (with-handlers 
      ([exn:fail? (lambda (e) 
                    (displayln (format "Error: ~a" (exn-message e)))
                    (void))])
      (let* ((input-port (open-input-string source-code))
             (ast (full-parser (lambda () (full-lexer input-port)))))
        (let ((result (value-of-program ast)))
          (displayln (format "Result: ~s" result))
          result)))))

(displayln "----------fib printer----------")
(test-parse-and-interpret "
        int fib(int n) {
                if (n == 0 || n == 1) {
                        return 1;
                } else {
                        return fib(n - 1) + fib(n - 2);
                }
        };

        int n = 10;
        int idx = 1;
        while (idx <= n) {
                $print(idx);
                $print(\": \");
                $print(fib(idx));
                $print(\"----------------\");

                idx = idx + 1;
        }
") 

(displayln "------------- fib DP printer -------------")
(test-parse-and-interpret "
        int n = 10;
        list dp;
        $push(dp, 1);
        $push(dp, 1);

        int idx = 2;
        int n = 10;
        while (idx <= n) {
                int ans = $get(dp, (idx - 1)) + $get(dp, (idx - 2));
                $print(idx);
                $print(\": \");
                $print(ans);
                $print(\"----------------\");

                idx = idx + 1;
                $push(dp, ans);
        }
") 


(displayln "------------- To Upper: -------------")
(test-parse-and-interpret "
        string s = \"saLam Sal4m\";
        list schars = $tocharlist(s);
        int sz = $size(schars);
        $print(sz);
        string result = \"\";
        int idx = 0;
        while (idx < sz) {
                char c = $get(schars, idx);
                if (c == 'a') {
                c = 'A';
                }
                if (c == 'b') {
                c = 'B';
                }
                if (c == 'c') {
                c = 'C';
                }
                if (c == 'd') {
                c = 'D';
                }
                if (c == 'e') {
                c = 'E';
                }
                if (c == 'f') {
                c = 'F';
                }
                if (c == 'g') {
                c = 'G';
                }
                if (c == 'h') {
                c = 'H';
                }
                if (c == 'i') {
                c = 'I';
                }
                if (c == 'j') {
                c = 'J';
                }
                if (c == 'k') {
                c = 'K';
                }
                if (c == 'l') {
                c = 'L';
                }
                if (c == 'm') {
                c = 'M';
                }
                if (c == 'n') {
                c = 'N';
                }
                if (c == 'o') {
                c = 'O';
                }
                if (c == 'p') {
                c = 'P';
                }
                if (c == 'q') {
                c = 'Q';
                }
                if (c == 'r') {
                c = 'R';
                }
                if (c == 's') {
                c = 'S';
                }
                if (c == 't') {
                c = 'T';
                }
                if (c == 'u') {
                c = 'U';
                }
                if (c == 'v') {
                c = 'V';
                }
                if (c == 'w') {
                c = 'W';
                }
                if (c == 'x') {
                c = 'X';
                }
                if (c == 'y') {
                c = 'Y';
                }
                if (c == 'z') {
                c = 'Z';
                }

                result = result + c;
                idx = idx + 1;
        }

        $print(result);
") 