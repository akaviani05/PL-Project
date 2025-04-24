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

(define sample-code2
	"
	int a = ~10 + 11 * 6 / 10 ^ 5 & 3;
	$print(a);
	$input(a);
	$print(a);

	while(a > 0 && a != 43){a = !a}

	int b;
	float c = .12;

	if(a == 0){
		a = 0;
	} else if(a == 1 && b == .0 || c + a <= a & b | ~b){
		a = 1;
	} else {
		a = 2;
	}

	list<list<bool>> yep;
	list<bool> nop;

	$push(nop , false);
	$push(nop , true);

	$push(yep , nop);

	$pop(nop);
	$push(nop , true);

	$push(yep , nop);

	$print($get($get(yep , 1 , 1));

	"
)

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