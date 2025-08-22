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


(run-demo "XO Game:"
          "
          $print(\"Input format for every row: CCCCCC (6 chars: -, X, or O)\");
          list G;
          int i = 0;
          int N = 6;
          while (i < N) {
             string row;
             $input(row);
             $push(G, $tocharlist(row));
             i = i + 1;
          }

          bool x_win = false;
          bool o_win = false;
          bool still_empty = false;
          
          
          // Check for 4 consecutive in rows
          i = 0;
          while (i < N) {
             int j = 0;
             while (j < N - 3) {
                list row = $get(G, i);
                char c1 = $get(row, j);
                char c2 = $get(row, j + 1);
                char c3 = $get(row, j + 2);
                char c4 = $get(row, j + 3);
                
                if (c1 == 'X' && c2 == 'X' && c3 == 'X' && c4 == 'X') {
                   x_win = true;
                }
                if (c1 == 'O' && c2 == 'O' && c3 == 'O' && c4 == 'O') {
                   o_win = true;
                }
                j = j + 1;
             }
             i = i + 1;
          }
          
          char XX = 'X';
          char OO = 'O';

          // Check for 4 consecutive in columns
          j = 0;
          while (j < N) {
             i = 0;
             while (i < N - 3) {
                list row1 = $get(G, i);
                list row2 = $get(G, i + 1);
                list row3 = $get(G, i + 2);
                list row4 = $get(G, i + 3);
                
                char c1 = $get(row1, j);
                char c2 = $get(row2, j);
                char c3 = $get(row3, j);
                char c4 = $get(row4, j);

                $print(c1);
                $print(c2);
                $print(c3);
                $print(c4);
                $print(\"___\");
                
                if (c1 == 'X' && c2 == 'X' && c3 == 'X' && c4 == 'X') {
                   x_win = true;
                }
                if (c1 == 'O' && c2 == 'O' && c3 == 'O' && c4 == 'O') {
                   o_win = true;
                }
                i = i + 1;
             }
             j = j + 1;
          }
          
          // Check main diagonals (top-left to bottom-right)
          i = 0;
          while (i < N - 3) {
             j = 0;
             while (j < N - 3) {
                list row1 = $get(G, i);
                list row2 = $get(G, i + 1);
                list row3 = $get(G, i + 2);
                list row4 = $get(G, i + 3);
                
                char c1 = $get(row1, j);
                char c2 = $get(row2, j + 1);
                char c3 = $get(row3, j + 2);
                char c4 = $get(row4, j + 3);
                
                if (c1 == 'X' && c2 == 'X' && c3 == 'X' && c4 == 'X') {
                   x_win = true;
                }
                if (c1 == 'O' && c2 == 'O' && c3 == 'O' && c4 == 'O') {
                   o_win = true;
                }
                j = j + 1;
             }
             i = i + 1;
          }
          
          // Check anti-diagonals (top-right to bottom-left)
          i = 0;
          while (i < N - 3) {
             j = 3;
             while (j < N) {
                list row1 = $get(G, i);
                list row2 = $get(G, i + 1);
                list row3 = $get(G, i + 2);
                list row4 = $get(G, i + 3);
                
                char c1 = $get(row1, j);
                char c2 = $get(row2, j - 1);
                char c3 = $get(row3, j - 2);
                char c4 = $get(row4, j - 3);

                if (c1 == 'X' && c2 == 'X' && c3 == 'X' && c4 == 'X') {
                   x_win = true;
                }
                if (c1 == 'O' && c2 == 'O' && c3 == 'O' && c4 == 'O') {
                   o_win = true;
                }
                j = j + 1;
             }
             i = i + 1;
          }
          
          // Check for empty spaces
          i = 0;
          while (i < N) {
             j = 0;
             while (j < N) {
                list row = $get(G, i);
                char cell = $get(row, j);
                if (cell == '-') {
                   still_empty = true;
                }
                j = j + 1;
             }
             i = i + 1;
          }
          
          $print(\"Debug results:\");
          $print(x_win);
          $print(o_win);
          $print(still_empty);
          
          // Determine and print the result
          if (x_win && o_win) {
             $print(\"Somebody cheated!\");
          } else if (x_win) {
             $print(\"X wins!\");
          } else if (o_win) {
             $print(\"O wins!\");
          } else if (still_empty) {
             $print(\"Game is still to be continued\");
          } else {
             $print(\"Draw!\");
          }

          XX;

          ")