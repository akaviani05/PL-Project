%token ID
%token INTVAL
%token FLOATVAL
%token STRVAL
%token CHARVAL
%token BOOLVAL
%start (program)
%%
program: statement-seq 
;
statement-seq: 
| statement-seq statement 
| statement-seq scope 
;
statement: simple-stament 'SEMICOLON' 
| if-statement 
| while-statement 
;
scope: 'LBRACE' statement-seq 'RBRACE' 
;
simple-stament: var-declaration 
| function-declaration 
| expression 
| break-statement 
| continue-statement 
| return-statement 
| predefined-statement 
;
if-statement: 'IF' 'LPAREN' expression 'RPAREN' scope 
| 'IF' 'LPAREN' expression 'RPAREN' scope 'ELSE' scope 
| 'IF' 'LPAREN' expression 'RPAREN' scope 'ELSE' if-statement 
;
while-statement: 'WHILE' 'LPAREN' expression 'RPAREN' scope 
;
var-declaration: var-type-name 
| var-type-name 'ASSIGN' expression 
;
var-type-name: var-type ID 
;
function-declaration: var-type ID 'LPAREN' 'RPAREN' scope 
| var-type ID 'LPAREN' var-seq 'RPAREN' scope 
;
var-seq: var-type-name 
| var-seq 'COMMA' var-type-name 
;
assignment: ID 'ASSIGN' expression 
;
break-statement: 'BREAK' 
;
continue-statement: 'CONTINUE' 
;
return-statement: 'RETURN' 
| 'RETURN' expression 
;
atom: predefined-statement 
| function-call 
| assignment 
| ID 
| value 
;
function-call: ID 'LPAREN' 'RPAREN' 
| ID 'LPAREN' expression-seq 'RPAREN' 
;
expression-seq: expression 
| expression-seq 'COMMA' expression 
;
predefined-statement: 'PRINT' 'LPAREN' expression 'RPAREN' 
| 'INPUT' 'LPAREN' ID 'RPAREN' 
| 'GET' 'LPAREN' ID 'COMMA' expression 'RPAREN' 
| 'SET' 'LPAREN' ID 'COMMA' expression 'COMMA' expression 'RPAREN' 
| 'PUSH' 'LPAREN' ID 'COMMA' expression 'RPAREN' 
| 'POP' 'LPAREN' ID 'RPAREN' 
;
exp0: atom 
| 'LPAREN' exp7 'RPAREN' 
;
exp1: 'NOT' exp0 
| 'TILDE' exp0 
| exp0 
;
exp2: exp2 'TIMES' exp1 
| exp2 'DIVIDE' exp1 
| exp2 'MOD' exp1 
| exp1 
;
exp3: exp3 'PLUS' exp2 
| exp3 'MINUS' exp2 
| exp2 
;
exp4: exp4 'LE' exp3 
| exp4 'GE' exp3 
| exp4 'LT' exp3 
| exp4 'GT' exp3 
| exp4 'EQ' exp3 
| exp4 'NE' exp3 
| exp3 
;
exp5: exp5 'AMP' exp4 
| exp5 'CARET' exp4 
| exp5 'PIPE' exp4 
| exp4 
;
exp6: exp6 'AND' exp5 
| exp5 
;
exp7: exp7 'OR' exp6 
| exp6 
;
expression: exp7 
;
var-type: 'INT' 
| 'FLOAT' 
| 'STRING' 
| 'CHAR' 
| 'BOOL' 
| 'LIST' 'LT' var-type 'GT' 
;
value: INTVAL 
| FLOATVAL 
| CHARVAL 
| STRVAL 
| BOOLVAL 
| ID 
;
%%
