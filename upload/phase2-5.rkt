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

(run-demo "Mentors"
          "



            list create(int stuid, string first, string last, float avg, int parid) {
                list stu;
                $push(stu, stuid);
                $push(stu, first);
                $push(stu, last);
                $push(stu, avg);
                $push(stu, parid);

                return stu;
            };
          
            int n;
            n = 400;
            list students;
            $push(students, create(402111111, \"person4\", \"test4\", 18.05, -1));
            $push(students, create(402222222, \"person5\", \"test5\", 18.29, -1));
            $push(students, create(403111111, \"person6\", \"test6\", 14.84, -1));
            $push(students, create(401111111, \"person3\", \"test3\", 19.72, 402111111));
            $push(students, create(99111111, \"person0\", \"test0\", 18.98, 403111111));
            $push(students, create(400111111, \"person1\", \"test1\", 18.12, 401111111));
            $push(students, create(400222222, \"person2\", \"test2\", 19, 402222222));

            int tn = $size(students);
            int i = 1;
            while (i < tn) {
                int j = i - 1;
                while (j >= 0) {
                    list prv = $get(students, j);
                    list nxt = $get(students, j + 1);
                    if ($get(prv, 0) > $get(nxt, 0)) {
                        $set(students, j, nxt);
                        $set(students, j + 1, prv);
                    } else {
                        j = 0;
                    }
                    j = j - 1;
                }
                i = i + 1;
            }

            list get_student(list students, int id) {
                int i = 0;
                list result;
                while (i < $size(students)) {
                    list student = $get(students, i);
                    if ($get(student, 0) == id) {
                        result = student;
                    }
                    i = i + 1;
                }
                return result;
            };
            int max_gpa_id = -1;
            float max_gpa = -1.0;
            i = tn - 1;
            while (i >= 0) {
                list student = $get(students, i);
                float cnt = 0;
                float gpa_sum = 0;
                if ($get(student, 4) != -1) {
                    list mentored = get_student(students, $get(student, 4));
                    cnt = 1 + $get(mentored, 5);
                    gpa_sum = $get(mentored, 3) + $get(mentored, 6);
                }
                int year = $get(student, 0) / 1000000;
                if (year == n) {
                    if (cnt > 0) {
                        float avg = gpa_sum / cnt;
                        if (avg > max_gpa) {
                            max_gpa = avg;
                            max_gpa_id = $get(student, 0);
                        }
                    }
                }
                $push(student, cnt);
                $push(student, gpa_sum);
                $set(students, i, student);
                i = i - 1;
            }

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

            list top_student = get_student(students, max_gpa_id);
            $print(concat(concat($get(top_student, 1), \" \"), $get(top_student, 2)));
            $print(max_gpa);


                       

          ")

(displayln "\n=== Test completed ===")

;;;         $print(\"Input the number of students:\");
;;;         int n;
;;;         $input(n);
;;;         list students;
;;;         int i = 0;
;;;         $print(\"Input fromat for each student: int, string, string, float, -1 or id of mentored\");
;;;         while (i < n) { 
;;;             int id;
;;;             string first_name;
;;;             string last_name;
;;;             float gpa;
;;;             int mentor_id;
;;;             $input(id);
;;;             $input(first_name);
;;;             $input(last_name);
;;;             $input(gpa);
;;;             $input(mentor_id);
;;;             list student;
;;;             $push(student, id);
;;;             $push(student, first_name);
;;;             $push(student, last_name);
;;;             $push(student, gpa);
;;;             $push(student, mentor_id);
;;;             $push(students, student);
;;;             $push(student, 0);
;;;             $push(student, 0);
;;;             i = i + 1;
;;;         }