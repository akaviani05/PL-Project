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
(run "Reverse:"
          "
          // BEGIN INPUTS
          int x = 123;
          // END INPUTS 

          
          if (x < 0) {
              $print(\"no support for negative numbers\");
          } else {
              int result = 0;
              while (x > 0) {
                result = result * 10 + x % 10;
                x = x / 10;
              }

              $print(result);
          }
          ")
