#lang racket

(require parser-tools/lex) ; Required for token-name
(require "lexer.rkt")     ; Your lexer file

(define sample-code
  "
  3 * 4 + 5 * 6 / 7 + 2
  ")

; --- Lexing the input string ---
(define in (open-input-string sample-code))
(define tokens '()) ; List to store the tokens

; Loop to get all tokens until EOF
(let loop ()
  (define current-token (full-lexer in))
  (set! tokens (cons current-token tokens))
  ; Stop when the EOF token is encountered
  (unless (eq? (token-name current-token) 'EOF)
    (loop)))

(close-input-port in)
(define final-token-list (reverse tokens)) ; Reverse to get the correct order

; --- Displaying the results ---
(displayln "Input String:")
(displayln sample-code)
(displayln "\nLexed Tokens:")
(pretty-print final-token-list) ; Use pretty-print for better formatting