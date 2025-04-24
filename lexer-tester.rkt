#lang racket

(require parser-tools/lex)
(require "lexer.rkt") 

(define sample-code1 
  "
    int a = 10;
    float b = 12.12;
    a = $input() + 5;
    true;
    string test = \"salam\";
    char test2 = 'c';

    int add(int a, int b) {
      return a + b;
    }

    int factorial(int a, int b) {
      return a + b;
    }

    // test
  "
)

;;; (define sample-code2
;;;   "
;;;   int a;
;;;   int b;
;;;   int c;
;;;   a = $input() + 5;
;;;   b = a + (b * b * b);

;;;   int add(int a, int b) {
;;;     return a + b;
;;;   }

;;;   c = add(a, b) + 2;
;;;   if (c > 10) {
;;;     $print("hello there");
;;;   } else {
;;;     $print("goodbye");
;;;   }

;;;   while (c < 20) {
;;;     c = c + 1;
;;;   }
;;;   "
;;; )

(define in (open-input-string sample-code1))
(define tokens '())

(let loop ()
  (define current-token (full-lexer in))
  (set! tokens (cons current-token tokens))
  (unless (eq? (token-name current-token) 'EOF)
	(loop)))

(close-input-port in)
(define final-token-list (reverse tokens)) 

(displayln "Input String:")
(displayln sample-code1)
(displayln "\nLexed Tokens:")
(pretty-print final-token-list)