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
(run-demo "Dates"
          "
          
          list dates;
          $push(dates, \"020250112\");
          $push(dates, \"20190418\");
          $push(dates, \"20230928\");
          $push(dates, \"20241515\");
          $push(dates, \"20210101\");
          $push(dates, \"20001231\");
          $push(dates, \"20250618\");
          
          $push(dates, \"20250632\");
         $push(dates, \"19701101\");
          
          

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
            int ans = 0;
            if (c == '0') {ans = 0;}
            if (c == '1') {ans = 1;}
            if (c == '2') {ans = 2;}
            if (c == '3') {ans = 3;}
            if (c == '4') {ans = 4;}
            if (c == '5') {ans = 5;}
            if (c == '6') {ans = 6;}
            if (c == '7') {ans = 7;}
            if (c == '8') {ans = 8;}
            if (c == '9') {ans = 9;}

            return ans;
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

          
          int sz = $size(dates);
          int bi = 0;
          while (bi < sz) {
                int bj = 0;
                while (bj < (sz - 1)) {
                    string cur = $get(dates, bj);
                    string nxt = $get(dates, bj + 1);

                    if (strtoint(cur) > strtoint(nxt)) {
                        $set(dates, bj, nxt);
                        $set(dates, bj + 1, cur);
                    }
                    
                    bj = bj + 1;
                }

                bi = bi + 1;
          } 

          
          list validDates;
          i = 0;

          while (i < $size(dates)) {
              string currentDate = $get(dates, i);
              
              
              list cht = $tocharlist(currentDate);
              int sz = $size(cht);
              if (sz == 8) {
                  
                  list chars = $tocharlist(currentDate);
                  
                  
                  string year = \"\";
                  int j = 0;
                  while (j < 4) {
                      char c = $get(chars, j);
                      year = year + c;
                      j = j + 1;
                  }
                  
                  
                  string monthStr = \"\";
                  j = 4;
                  while (j < 6) {
                      char c = $get(chars, j);
                      monthStr = monthStr + c;
                      j = j + 1;
                  }
                  
                  
                  string dayStr = \"\";
                  j = 6;
                  while (j < 8) {
                      char c = $get(chars, j);
                      dayStr = dayStr + c;
                      j = j + 1;
                  }
                  
                  
                  int month = 0;
                  if (monthStr == \"01\") { month = 1; }
                  if (monthStr == \"02\") { month = 2; }
                  if (monthStr == \"03\") { month = 3; }
                  if (monthStr == \"04\") { month = 4; }
                  if (monthStr == \"05\") { month = 5; }
                  if (monthStr == \"06\") { month = 6; }
                  if (monthStr == \"07\") { month = 7; }
                  if (monthStr == \"08\") { month = 8; }
                  if (monthStr == \"09\") { month = 9; }
                  if (monthStr == \"10\") { month = 10; }
                  if (monthStr == \"11\") { month = 11; }
                  if (monthStr == \"12\") { month = 12; }
                  
                  
                  int day = 0;
                  if (dayStr == \"01\") { day = 1; }
                  if (dayStr == \"02\") { day = 2; }
                  if (dayStr == \"03\") { day = 3; }
                  if (dayStr == \"04\") { day = 4; }
                  if (dayStr == \"05\") { day = 5; }
                  if (dayStr == \"06\") { day = 6; }
                  if (dayStr == \"07\") { day = 7; }
                  if (dayStr == \"08\") { day = 8; }
                  if (dayStr == \"09\") { day = 9; }
                  if (dayStr == \"10\") { day = 10; }
                  if (dayStr == \"11\") { day = 11; }
                  if (dayStr == \"12\") { day = 12; }
                  if (dayStr == \"13\") { day = 13; }
                  if (dayStr == \"14\") { day = 14; }
                  if (dayStr == \"15\") { day = 15; }
                  if (dayStr == \"16\") { day = 16; }
                  if (dayStr == \"17\") { day = 17; }
                  if (dayStr == \"18\") { day = 18; }
                  if (dayStr == \"19\") { day = 19; }
                  if (dayStr == \"20\") { day = 20; }
                  if (dayStr == \"21\") { day = 21; }
                  if (dayStr == \"22\") { day = 22; }
                  if (dayStr == \"23\") { day = 23; }
                  if (dayStr == \"24\") { day = 24; }
                  if (dayStr == \"25\") { day = 25; }
                  if (dayStr == \"26\") { day = 26; }
                  if (dayStr == \"27\") { day = 27; }
                  if (dayStr == \"28\") { day = 28; }
                  if (dayStr == \"29\") { day = 29; }
                  if (dayStr == \"30\") { day = 30; }
                  if (dayStr == \"31\") { day = 31; }
                  
                  
                  if (month >= 1 && month <= 12 && day >= 1 && day <= 31) {
                      
                      string monthName = \"\";
                      if (month == 1) { monthName = \"Jan\"; }
                      if (month == 2) { monthName = \"Feb\"; }
                      if (month == 3) { monthName = \"Mar\"; }
                      if (month == 4) { monthName = \"Apr\"; }
                      if (month == 5) { monthName = \"May\"; }
                      if (month == 6) { monthName = \"Jun\"; }
                      if (month == 7) { monthName = \"Jul\"; }
                      if (month == 8) { monthName = \"Aug\"; }
                      if (month == 9) { monthName = \"Sep\"; }
                      if (month == 10) { monthName = \"Oct\"; }
                      if (month == 11) { monthName = \"Nov\"; }
                      if (month == 12) { monthName = \"Dec\"; }
                      
                      
                      string formattedDate = concat(concat(concat(concat(monthName, \" \"), dayStr), \" of \"), year);
                      $print(formattedDate);
                  }
              }
              i = i + 1;
          }
          

          ")

(displayln "\n=== Test completed ===")
