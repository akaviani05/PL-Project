#lang racket

;; TODO: Define Bool

(require parser-tools/lex
         (prefix-in : parser-tools/lex-sre))


(define-lex-abbrev DIGIT 
  (char-range #\0 #\9)
)

(define-lex-abbrev NUMBER 
  (:+ DIGIT)
)

(define-lex-abbrev SIGNER
  (:+ (:or #\+ #\-))
)

(define-lex-abbrev INTVAL
  (:: (:? SIGNER) NUMBER)
)

(define-lex-abbrev FLOATVAL
  (:: (:? INTVAL) #\. NUMBER)
)

(define-lex-abbrev LETTER 
  (:or (char-range #\a #\z) 
        (char-range #\A #\Z) 
        #\_)
)

(define-lex-abbrev WORD 
  (:: LETTER (:* (:or LETTER DIGIT)))
)


(define-lex-abbrev CHARVAL
  (:: #\'
      (:or (:~ (:or #\\ #\'))                    
           (:: #\\ (:or #\\ #\' #\" #\n #\t #\r))) 
      #\')
)

(define-lex-abbrev STRVAL
  (:: #\"
      (:* (:or (:~ (:or #\\ #\"))                  
               (:: #\\ (:or #\\ #\' #\" #\n #\t #\r))))
      #\")
)

(define-lex-abbrev BOOLVAL
  (:or "true" "false")
)

(define full-lexer
  (lexer
   [(:or #\space #\tab #\newline #\return) (full-lexer input-port)]
   [(:: "//" (:* (:~ #\newline))) (full-lexer input-port)]

   ["if" (token-IF)]
   ["else" (token-ELSE)]
   ["while" (token-WHILE)]
   ["break" (token-BREAK)]
   ["continue" (token-CONTINUE)]
   ["return" (token-RETURN)]
   ["int" (token-INT)]
   ["float" (token-FLOAT)]
   ["string" (token-STRING)]
   ["char" (token-CHAR)]
   ["bool" (token-BOOL)]
   ["list" (token-LIST)]

   ["$print" (token-PRINT)]
   ["$input" (token-INPUT)]
   ["$get" (token-GET)]
   ["$set" (token-SET)]
   ["$push" (token-PUSH)]
   ["$pop" (token-POP)]
   ["$tocharlist" (token-TOCHARLIST)]

   ["{" (token-LBRACE)]
   ["}" (token-RBRACE)]
   ["(" (token-LPAREN)]
   [")" (token-RPAREN)]
   [";" (token-SEMICOLON)]
   ["," (token-COMMA)]
   ["<" (token-LT)]
   [">" (token-GT)]

   ["=" (token-ASSIGN)]
   ["+" (token-PLUS)]
   ["-" (token-MINUS)]
   ["*" (token-TIMES)]
   ["/" (token-DIVIDE)]
   ["%" (token-MOD)]
   ["<=" (token-LE)]
   [">=" (token-GE)]
   ["==" (token-EQ)]
   ["!=" (token-NE)]
   ["!" (token-NOT)]
   ["~" (token-TILDE)]
   ["&" (token-AMP)]
   ["^" (token-CARET)]
   ["|" (token-PIPE)]
   ["&&" (token-AND)]
   ["||" (token-OR)]

   ;;; TODO: true and fale
   [INTVAL (token-INTVAL (string->number lexeme))]
   [FLOATVAL (token-FLOATVAL (string->number lexeme))]
   [CHARVAL (token-CHARVAL lexeme)]
   [STRVAL (token-STRVAL lexeme)]
   [BOOLVAL (token-BOOLVAL lexeme)]
   [WORD (token-ID lexeme)]
   
   
   [(eof) (token-EOF)]
))

(define-tokens value-tokens (ID INTVAL FLOATVAL STRVAL CHARVAL BOOLVAL))

(define-empty-tokens empty-tokens
  (IF ELSE WHILE BREAK CONTINUE RETURN
   LIST INT FLOAT STRING CHAR BOOL
   PRINT INPUT GET SET PUSH POP TOCHARLIST
   LBRACE RBRACE LPAREN RPAREN LT GT SEMICOLON COMMA
   ASSIGN PLUS MINUS TIMES DIVIDE MOD LE GE EQ NE NOT TILDE AMP CARET PIPE AND OR
   EOF))

(provide (all-defined-out))