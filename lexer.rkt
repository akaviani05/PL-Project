#lang racket

(require parser-tools/lex
         (prefix-in : parser-tools/lex-sre)
         parser-tools/yacc)

(define full-lexer (lexer
   (whitespace (full-lexer input-port))
   ((:or (:: (:+ (char-range #\0 #\9))) (:: (:: (:+ (char-range #\0 #\9)) #\. (:+ (char-range #\0 #\9))))) (token-NUM (string->number lexeme)))
   ((eof) (token-EOF))
   ("+" (token-PLUS))
   ("-" (token-MINUS))
   ("*" (token-TIMES))
   ("/" (token-DIVIDES))
   ((:+ (:or (char-range #\0 #\9) (char-range #\a #\z) (char-range #\A #\Z) #\_)) (token-ID lexeme))))

(define-tokens a (NUM ID))
(define-empty-tokens b (EOF SEMICOLON PASS BREAK CONTINUE ASSIGNMENT RETURN GLOBAL DEF OP CP COLON COMMA TRUE FALSE
                        IF ELSE FOR IN OR AND NOT EQUALS LT LET GT GET PLUS MINUS TIMES DIVIDES POWER OB CB NONE))

(provide (all-defined-out))
