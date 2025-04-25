;;; Spc -> Space | Tab | NewLine


;;; Program -> StatementSeq
;;; SimpleStatement -> VarDec | FunDec | Assignment | Expression | BreakStatement | ContinueStatement | ReturnStatemnt | PreDefinedStatement
;;; Statement -> SimpleStatement; | IfStatement | WhileStatement
;;; StatementSeq -> StatementSeq Statement | StatementSeq Scope | empty
;;; Scope -> { StatementSeq }
;;; Expression

;;; IfStatement -> if ( Expression ) Scope | if ( Expression ) Scope else Scope | if ( Expression ) Scope else IfStatement
;;; WhileStatement -> while ( Expression ) Scope

;;; Word -> "(a-z,A-Z,_)(a-z,A-Z,_,0-9)*"
;;; VarName -> Word
;;; FunName -> Word
;;; VarType -> int | float | string | char | bool | list<VarType> 

;;; VarTypeName -> VarType Spc VarName

;;; VarDec -> VarTypeName | VarTypeName = Expression

;;; VarSeq -> VarTypeName , VarSeq | VarTypeName

;;; FunDec -> VarType Spc FunName ( ) Scope | VarType Spc FunName ( VarSeq ) Scope

;;; ExpSeq -> Expression | Expression , ExpSeq

;;; FunCal -> FunName ( ExpSeq ) | FunName ( )

;;; BreakStatement -> break
;;; ContinueStatement -> continue
;;; ReturnStatemnt -> return | return Spc Expression

;;; Assignment -> VarName = Expression

;;; Op1 -> ! | ~
;;; Op2 -> * | / | %
;;; Op3 -> + | -
;;; Op4 -> <= | >= | < | > | == | !=
;;; Op5 -> & | ^ | |
;;; Op6 -> &&
;;; Op7 -> ||

;;; Exp0 -> Atom | ( EXP7 )
;;; Exp1 -> Op1 Exp0 | Exp0
;;; Exp2 -> Exp2 Op2 Exp1 | Exp1
;;; Exp3 -> Exp3 Op3 Exp2 | Exp2
;;; Exp4 -> Exp4 Op4 Exp3 | Exp3
;;; Exp5 -> Exp5 Op4 Exp4 | Exp4
;;; Exp6 -> Exp6 Op4 Exp5 | Exp5
;;; Exp7 -> Exp7 Op4 Exp6 | Exp6

;;; Expression -> Exp7

;;; Atom -> PreDefinedStatement | FunCal | VarName | Value

;;; Value -> BoolVal | IntVal | FloatVal | CharVal | StrVal

;;; BoolVal -> false | true
;;; IntVal -> (+-)?(0-9)+
;;; FloatVal -> IntVal | (+-)?(0-9)*\.(0-9)+
;;; CharVal -> '.' | '\\.'
;;; StrVal -> "" | ".*(non \)"

;;; PreDefinedStatement = PrintStatement | InputStatement | GetStatement | SetStatement | PushStatement | PopStatement

;;; PrintStatement -> $print ( Expression )
;;; InputStatement -> $input ( VarName )
;;; GetStatement -> $get ( VarName , Expression )
;;; SetStatement -> $set ( VarName , Expression , Expression)
;;; PushStatement -> $push ( VarName , Expression)
;;; PopStatement -> $pop ( VarName )

;;; ================ parser.rkt ================

#lang racket

(require parser-tools/yacc "lexer.rkt")

(define (node tag . children) (cons tag children))

(define full-parser
  (parser
   (start program)
   (end   EOF)
   (error (lambda (tok? name val)
            (error 'parser (format "syntax error near ~a (~a)" name val))))

   (tokens value-tokens empty-tokens)

   (precs
      (right ASSIGN) ; =
      (left  OR)    ; ||
      (left  AND)    ; &&
      (left  AMP CARET PIPE)    ; & ^ |
      (nonassoc LE GE LT GT EQ NE) ; <= >= < > == !=
      (left  PLUS MINUS) ; + -
      (left  TIMES DIVIDE MOD) ; * / %
      (right NOT TILDE)) ; ! ~

   (grammar
      (program          ((statement-seq) (node 'program $1)))
      (statement-seq    (()                           '())
                        ((statement-seq statement)     (append $1 (list $2)))
                        ((statement-seq scope)         (append $1 (list $2))))
      (statement        ((simple-stament SEMICOLON)    (node 'simple-stament $1))
                        ((if-statement)                (node 'if-statement $1))
                        ((while-statement)             (node 'while-statement $1)))
      (scope            ((LBRACE statement-seq RBRACE) (node 'scope $2)))
      (simple-stament   ((var-declaration)             (node 'var-declaration $1))
                        ((function-declaration)        (node 'func-declaration $1))
                        ((expression)                  (node 'expression $1))
                        ((break-statement)             (node 'break-statement $1))
                        ((continue-statement)          (node 'continue-statement $1))
                        ((return-statement)            (node 'return-statement $1)))

      (if-statement     ((IF LPAREN expression RPAREN scope)                     (node 'if $3 $5))
                        ((IF LPAREN expression RPAREN scope ELSE scope)          (node 'if-else $3 $5 $7))
                        ((IF LPAREN expression RPAREN scope ELSE if-statement)   (node 'if-elseif $3 $5 $7)))
      
      (while-statement  ((WHILE LPAREN expression RPAREN scope) (node 'while $3 $5)))

      (var-declaration ((var-type-name)                    (node 'var-default $1))
                       ((var-type-name ASSIGN expression)  (node 'var-assign $1 $3)))

      (var-type-name ((var-type ID) (node 'var-type-name $1 $2)))

      (function-declaration ((var-type ID LPAREN RPAREN scope)          (node 'function-declaration $1 $2 $5 '()))
                            ((var-type ID LPAREN var-seq RPAREN scope)  (node 'function-declaration $1 $2 $6 $4)))

      (var-seq ((var-type-name)                '(list $1))
               ((var-seq COMMA var-type-name)  '(append $1 (list $3))))

      (assignment ((ID ASSIGN expression) (prec ASSIGN)       (node 'assignment $1 $3)))

      (break-statement ((BREAK)                  (node 'break-statement)))
      (continue-statement ((CONTINUE)            (node 'continue-statement)))
      (return-statement ((RETURN)                (node 'return-statement))
                        ((RETURN expression)     (node 'return-statement $2)))

      (atom ((predefined-statement)    (node 'predefined-atom $1))
            ((function-call)           (node 'function-atom $1))
            ((assignment)              (node 'assignment-atom $1))
            ((ID)                      (node 'var-name-atom $1))
            ((value)                   (node 'value-atom $1)))
      
      (function-call ((ID LPAREN RPAREN)         (node 'function-call $1 '()))
                     ((ID LPAREN expression-seq RPAREN) (node 'function-call $1 $3)))
      
      (expression-seq   ((expression)                       '(list $1))
                        ((expression-seq COMMA expression)  '(append $1 (list $3))))
      
      (predefined-statement
       ((PRINT LPAREN expression RPAREN)                         (node 'print-statement $3))
       ((INPUT LPAREN ID RPAREN)                                 (node 'input-statement $3))
       ((GET LPAREN ID COMMA expression RPAREN)                  (node 'get-statement $3 $5))
       ((SET LPAREN ID COMMA expression COMMA expression RPAREN) (node 'set-statement $3 $5 $7))
       ((PUSH LPAREN ID COMMA expression RPAREN)                 (node 'push-statement $3 $5))
       ((POP LPAREN ID RPAREN)                                   (node 'pop-statement $3)))

      (exp0 ((atom) (node 'atom $1))
            ((LPAREN expression RPAREN) (node 'paren $2)))
      
      (exp1 ((NOT exp0)    (node '! $2))
            ((TILDE exp0)  (node '~ $2))
            ((exp0)        (node 'exp0 $1)))
      
      (exp2 ((exp2 TIMES exp1)  (node '* $1 $3))
            ((exp2 DIVIDE exp1) (node '/ $1 $3))
            ((exp2 MOD exp1)    (node '% $1 $3))
            ((exp1)             (node 'exp1 $1)))

      (exp3 ((exp3 PLUS exp2)   (node '+ $1 $3))
            ((exp3 MINUS exp2)  (node '- $1 $3))
            ((exp2) (prec PLUS) (node 'exp2 $1)))

      (exp4 ((exp4 LE exp3)     (node '<= $1 $3))
            ((exp4 GE exp3)     (node '>= $1 $3))
            ((exp4 LT exp3)     (node '< $1 $3))
            ((exp4 GT exp3)     (node '> $1 $3))
            ((exp4 EQ exp3)     (node '== $1 $3))
            ((exp4 NE exp3)     (node '!= $1 $3))
            ((exp3) (prec LE)   (node 'exp3 $1)))

      (exp5 ((exp5 AMP exp4)    (node '& $1 $3))
            ((exp5 CARET exp4)  (node '^ $1 $3))
            ((exp5 PIPE exp4)   (node 'orop $1 $3))
            ((exp4) (prec AMP)  (node 'exp4 $1)))

      (exp6 ((exp6 AND exp5)    (node '&& $1 $3))
            ((exp5) (prec AND)  (node 'exp5 $1)))
      
      (expression ((expression OR exp6)   (node '|| $1 $3))
                  ((exp6) (prec OR)       (node 'exp6 $1)))


      (var-type  ((INT)                   (node 'int-t))
                 ((FLOAT)                 (node 'float-t))
                 ((STRING)                (node 'string-t))
                 ((CHAR)                  (node 'char-t))
                 ((BOOL)                  (node 'bool-t))
                 ((LIST LT var-type GT)   (node 'list-t $3)))

      (value   ((INTVAL)     (node 'int-val $1))
               ((FLOATVAL)   (node 'float-val $1))
               ((CHARVAL)    (node 'char-val $1))
               ((STRVAL)     (node 'str-val $1))
               ((BOOLVAL)    (node 'bool-val $1)))
    )
    
    (debug "parser-debug.txt")
    (yacc-output "parser.y")


    ))

(provide full-parser)
