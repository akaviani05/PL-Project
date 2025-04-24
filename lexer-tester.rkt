#lang racket

(require parser-tools/lex)
(require "lexer.rkt") 

(define sample-code1
  "
  int a;
  int b;
  int c;
  a = $input() + 5;
  b = a + (b * b * b);

  int add(int a, int b) {
    return a + b;
  }

  c = add(a, b) + 2;
  if (c > 10) {
    $print("hello there");
  } else {
    $print("goodbye");
  }

  while (c < 20) {
    c = c + 1;
  }
  ")

(define in (open-input-string sample-code))
(define tokens '())

(let loop ()
  (define current-token (full-lexer in))
  (set! tokens (cons current-token tokens))
  (unless (eq? (token-name current-token) 'EOF)
    (loop)))

(close-input-port in)
(define final-token-list (reverse tokens)) 

(displayln "Input String:")
(displayln sample-code)
(displayln "\nLexed Tokens:")
(pretty-print final-token-list)