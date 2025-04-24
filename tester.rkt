#lang racket

(require "lexer.rkt") ; Assuming lexer.rkt is in the same directory

; --- Test Function ---
(define (test-lexer input-string)
  (define in (open-input-string input-string))
  (define tokens '())
  (let loop ()
    (define token (full-lexer in))
    (set! tokens (cons token tokens))
    (unless (equal? token (token-EOF)) ; Check if the token is EOF
      (loop)))
  (close-input-port in)
  (reverse tokens)) ; Reverse to get the original order

; --- Example Usage ---
(define test-string "123 + 45.67 * abc_1")
(define resulting-tokens (test-lexer test-string))

(displayln "Input String:")
(displayln test-string)
(displayln "\nResulting Tokens:")
(pretty-print resulting-tokens)

; --- Expected Output (for comparison) ---
; (list (token-NUM 123) (token-PLUS) (token-NUM 45.67) (token-TIMES) (token-ID "abc_1") (token-EOF))

