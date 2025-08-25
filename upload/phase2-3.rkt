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

(run "XO:"
          "
          // Input format for every row: CCCCCC (6 chars: -, X, or O)
          // BEGIN INPUT
          string row1 = \"-X--XO\";
          string row2 = \"OOX--O\";
          string row3 = \"--XX-O\";
          string row4 = \"-XXOXO\";
          string row5 = \"OO--X-\";
          string row6 = \"------\";
         // END INPUT
          list G;
          $push(G, $tocharlist(row1));
          $push(G, $tocharlist(row2));
          $push(G, $tocharlist(row3));
          $push(G, $tocharlist(row4));
          $push(G, $tocharlist(row5));
          $push(G, $tocharlist(row6));
          

          // int i = 0;
         int N = 6;
         // while (i < N) {
          //   string row;
          //   $input(row);
          //   $push(G, $tocharlist(row));
          //   i = i + 1;
         //  }

          bool x_win = false;
          bool o_win = false;
          bool still_empty = false;
          
          
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