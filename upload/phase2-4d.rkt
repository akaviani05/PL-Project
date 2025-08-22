#lang racket

(require "interpreter.rkt")
(require "lexer.rkt")
(require "parser.rkt")
(require "datatypes.rkt")

(define run-demo
  (lambda (description source-code)
    (displayln "")
    (displayln (format "=== ~a ===" description))
    (displayln (format "Code: ~a" source-code))
    (displayln "Output:")
    (let* ((input-port (open-input-string source-code))
           (ast (full-parser (lambda () (full-lexer input-port)))))
      (let ((result (value-of-program ast)))
        (displayln (format "Final Result: ~s" result))
        (with-handlers 
          ([exn:fail? (lambda (e) (displayln (format "→ Raw: ~s" result)))])
          (cond
            [(with-handlers ([exn:fail? (lambda (e) #f)]) 
               (let ((num (expval->num result))) 
                 (displayln (format "→ Integer: ~a" num)) #t)) #t]
            [(with-handlers ([exn:fail? (lambda (e) #f)]) 
               (let ((bool (expval->bool result))) 
                 (displayln (format "→ Boolean: ~a" bool)) #t)) #t]
            [(with-handlers ([exn:fail? (lambda (e) #f)]) 
               (let ((str (expval->string result))) 
                 (displayln (format "→ String: ~a" str)) #t)) #t]
            [(with-handlers ([exn:fail? (lambda (e) #f)]) 
               (let ((fl (expval->float result))) 
                 (displayln (format "→ Float: ~a" fl)) #t)) #t]
            [else (displayln (format "→ Value: ~s" result))]))
        result))))

;;; READ: baraye in soal, fahmidim ye bug riz to $input darim(yekam sar type ha ahmagh bazi daravorde boodim) sare hamin majboor shodim voroodi ro predefined bezarim
(run-demo "Date Formatter and Validator:"
          "
          // BEGIN: CHANGE THIS PART TO YOUR DATES
          list dates;
          $push(dates, \"020250112\");
          $push(dates, \"20190418\");
          $push(dates, \"20230928\");
          $push(dates, \"20241515\");
          $push(dates, \"20210101\");
          $push(dates, \"20001231\");
          $push(dates, \"20250618\");
          
          // BEGIN: CHANGE THIS PART TO YOUR DATES

          string concat(string a, string b) {
                list cb = $tocharlist(b);
                int sz = $size(cb);
                int ind = 0;
                string result = a;
                while (ind < sz) {
                        result = result + $get(cb, ind);
                        ind = ind + 1;
                } 

                return result;
          };

          int chartonum(char c) {
            if (c == '0') {return 0;}
            if (c == '1') {return 1;}
            if (c == '2') {return 2;}
            if (c == '3') {return 3;}
            if (c == '4') {return 4;}
            if (c == '5') {return 5;}
            if (c == '6') {return 6;}
            if (c == '7') {return 7;}
            if (c == '8') {return 8;}
            if (c == '9') {return 9;}
          };


          int strtoint(string s) {
              list chl = $tocharlist(s);
              int sz = $size(chl);

              int i = 0;
              int ans = 0;
              while (i < sz) {
                char c = $get(chl, i);
                int x = chartonum(c);
                ans = ans * 10;
                ans = ans + x;

                i = i + 1;
              }

              return ans;
          };

          chartonum('2');
          ")

(displayln "\n=== Test completed ===")
