#lang racket

(require "interpreter.rkt")
(require "datatypes.rkt")

; Test helper function to run a program and print result
(define (test-program program-ast description)
  (display (string-append "=== " description " ==="))
  (newline)
  (let ((result (value-of-program program-ast)))
    (display "Result: ")
    (display (expval->string-for-print result))
    (newline)
    (newline)))

; Test 1: List declaration and initialization
(test-program
  '(program 
    ((simple-stament 
      (var-declaration 
        (var-assign 
          (var-type-name (list-t) "myList") 
          (exp6 (exp5 (exp4 (exp3 (exp2 (exp1 (exp0 (atom (value-atom (lst-val ((int-val 1) (int-val 2) (int-val 3) (bool-val "true") (str-val "hello")))))))))))))))))
  "List declaration with mixed types")

; Test 2: Empty list declaration
(test-program
  '(program 
    ((simple-stament 
      (var-declaration 
        (var-default 
          (var-type-name (list-t) "emptyList"))))))
  "Empty list declaration")

; Test 3: Print list
(test-program
  '(program 
    ((simple-stament 
      (var-declaration 
        (var-assign 
          (var-type-name (list-t) "testList") 
          (exp6 (exp5 (exp4 (exp3 (exp2 (exp1 (exp0 (atom (value-atom (lst-val ((int-val 10) (int-val 20) (int-val 30)))))))))))))))
     (simple-stament 
      (expression 
        (exp6 (exp5 (exp4 (exp3 (exp2 (exp1 (exp0 (atom (predefined-atom (print-statement (exp6 (exp5 (exp4 (exp3 (exp2 (exp1 (exp0 (atom (var-name-atom "testList")))))))))))))))))))))))
  "Print list contents")

; Test 4: Push operation
(test-program
  '(program 
    ((simple-stament 
      (var-declaration 
        (var-assign 
          (var-type-name (list-t) "pushList") 
          (exp6 (exp5 (exp4 (exp3 (exp2 (exp1 (exp0 (atom (value-atom (lst-val ((int-val 1) (int-val 2)))))))))))))))
     (simple-stament 
      (expression 
        (exp6 (exp5 (exp4 (exp3 (exp2 (exp1 (exp0 (atom (predefined-atom (push-statement "pushList" (exp6 (exp5 (exp4 (exp3 (exp2 (exp1 (exp0 (atom (value-atom (int-val 3))))))))))))))))))))))
     (simple-stament 
      (expression 
        (exp6 (exp5 (exp4 (exp3 (exp2 (exp1 (exp0 (atom (predefined-atom (print-statement (exp6 (exp5 (exp4 (exp3 (exp2 (exp1 (exp0 (atom (var-name-atom "pushList")))))))))))))))))))))))
  "Push element to list")

; Test 5: Get operation
(test-program
  '(program 
    ((simple-stament 
      (var-declaration 
        (var-assign 
          (var-type-name (list-t) "getList") 
          (exp6 (exp5 (exp4 (exp3 (exp2 (exp1 (exp0 (atom (value-atom (lst-val ((str-val "first") (str-val "second") (str-val "third")))))))))))))))
     (simple-stament 
      (expression 
        (exp6 (exp5 (exp4 (exp3 (exp2 (exp1 (exp0 (atom (predefined-atom (print-statement (exp6 (exp5 (exp4 (exp3 (exp2 (exp1 (exp0 (atom (predefined-atom (get-statement "getList" (exp6 (exp5 (exp4 (exp3 (exp2 (exp1 (exp0 (atom (value-atom (int-val 1))))))))))))))))))))))))))))))))))
  "Get element from list at index 1")

; Test 6: Set operation
(test-program
  '(program 
    ((simple-stament 
      (var-declaration 
        (var-assign 
          (var-type-name (list-t) "setList") 
          (exp6 (exp5 (exp4 (exp3 (exp2 (exp1 (exp0 (atom (value-atom (lst-val ((int-val 100) (int-val 200) (int-val 300)))))))))))))))
     (simple-stament 
      (expression 
        (exp6 (exp5 (exp4 (exp3 (exp2 (exp1 (exp0 (atom (predefined-atom (set-statement "setList" (exp6 (exp5 (exp4 (exp3 (exp2 (exp1 (exp0 (atom (value-atom (int-val 1)))))))))) (exp6 (exp5 (exp4 (exp3 (exp2 (exp1 (exp0 (atom (value-atom (int-val 999))))))))))))))))))))))
     (simple-stament 
      (expression 
        (exp6 (exp5 (exp4 (exp3 (exp2 (exp1 (exp0 (atom (predefined-atom (print-statement (exp6 (exp5 (exp4 (exp3 (exp2 (exp1 (exp0 (atom (var-name-atom "setList")))))))))))))))))))))))
  "Set element in list at index 1 to 999")

; Test 7: Pop operation - Fixed to work properly
(test-program
  '(program 
    ((simple-stament 
      (var-declaration 
        (var-assign 
          (var-type-name (list-t) "popList") 
          (exp6 (exp5 (exp4 (exp3 (exp2 (exp1 (exp0 (atom (value-atom (lst-val ((str-val "a") (str-val "b") (str-val "c") (str-val "d")))))))))))))))
     (simple-stament 
      (expression 
        (exp6 (exp5 (exp4 (exp3 (exp2 (exp1 (exp0 (atom (predefined-atom (print-statement (exp6 (exp5 (exp4 (exp3 (exp2 (exp1 (exp0 (atom (var-name-atom "popList")))))))))))))))))))))
     (simple-stament 
      (expression 
        (exp6 (exp5 (exp4 (exp3 (exp2 (exp1 (exp0 (atom (predefined-atom (pop-statement "popList"))))))))))))
     (simple-stament 
      (expression 
        (exp6 (exp5 (exp4 (exp3 (exp2 (exp1 (exp0 (atom (predefined-atom (print-statement (exp6 (exp5 (exp4 (exp3 (exp2 (exp1 (exp0 (atom (var-name-atom "popList")))))))))))))))))))))))
  "Pop last element from list and show remaining")

; Test 8: List equality comparison
(test-program
  '(program 
    ((simple-stament 
      (var-declaration 
        (var-assign 
          (var-type-name (list-t) "list1") 
          (exp6 (exp5 (exp4 (exp3 (exp2 (exp1 (exp0 (atom (value-atom (lst-val ((int-val 1) (int-val 2) (int-val 3)))))))))))))))
     (simple-stament 
      (var-declaration 
        (var-assign 
          (var-type-name (list-t) "list2") 
          (exp6 (exp5 (exp4 (exp3 (exp2 (exp1 (exp0 (atom (value-atom (lst-val ((int-val 1) (int-val 2) (int-val 3)))))))))))))))
     (simple-stament 
      (expression 
        (exp6 (exp5 (exp4 (exp3 (exp2 (exp1 (exp0 (atom (predefined-atom (print-statement (exp6 (exp5 (exp4 (== (exp3 (exp2 (exp1 (exp0 (atom (var-name-atom "list1")))))) (exp3 (exp2 (exp1 (exp0 (atom (var-name-atom "list2"))))))))))))))))))))))))
  "Compare two identical lists for equality")

; Test 9: Complex list operations sequence
(test-program
  '(program 
    ((simple-stament 
      (var-declaration 
        (var-assign 
          (var-type-name (list-t) "complexList") 
          (exp6 (exp5 (exp4 (exp3 (exp2 (exp1 (exp0 (atom (value-atom (lst-val ())))))))))))))
     (simple-stament 
      (expression 
        (exp6 (exp5 (exp4 (exp3 (exp2 (exp1 (exp0 (atom (predefined-atom (push-statement "complexList" (exp6 (exp5 (exp4 (exp3 (exp2 (exp1 (exp0 (atom (value-atom (str-val "hello"))))))))))))))))))))))
     (simple-stament 
      (expression 
        (exp6 (exp5 (exp4 (exp3 (exp2 (exp1 (exp0 (atom (predefined-atom (push-statement "complexList" (exp6 (exp5 (exp4 (exp3 (exp2 (exp1 (exp0 (atom (value-atom (int-val 42))))))))))))))))))))))
     (simple-stament 
      (expression 
        (exp6 (exp5 (exp4 (exp3 (exp2 (exp1 (exp0 (atom (predefined-atom (push-statement "complexList" (exp6 (exp5 (exp4 (exp3 (exp2 (exp1 (exp0 (atom (value-atom (bool-val "true"))))))))))))))))))))))
     (simple-stament 
      (expression 
        (exp6 (exp5 (exp4 (exp3 (exp2 (exp1 (exp0 (atom (predefined-atom (print-statement (exp6 (exp5 (exp4 (exp3 (exp2 (exp1 (exp0 (atom (var-name-atom "complexList")))))))))))))))))))))))
  "Build list from empty and add different types")

; Test 10: Nested list operations with variables
(test-program
  '(program 
    ((simple-stament 
      (var-declaration 
        (var-assign 
          (var-type-name (int-t) "index") 
          (exp6 (exp5 (exp4 (exp3 (exp2 (exp1 (exp0 (atom (value-atom (int-val 0)))))))))))))
     (simple-stament 
      (var-declaration 
        (var-assign 
          (var-type-name (list-t) "varList") 
          (exp6 (exp5 (exp4 (exp3 (exp2 (exp1 (exp0 (atom (value-atom (lst-val ((str-val "zero") (str-val "one") (str-val "two")))))))))))))))
     (simple-stament 
      (expression 
        (exp6 (exp5 (exp4 (exp3 (exp2 (exp1 (exp0 (atom (predefined-atom (print-statement (exp6 (exp5 (exp4 (exp3 (exp2 (exp1 (exp0 (atom (predefined-atom (get-statement "varList" (exp6 (exp5 (exp4 (exp3 (exp2 (exp1 (exp0 (atom (var-name-atom "index")))))))))))))))))))))))))))))))))
  "Get element using variable index")

; Test 11: Push list into another list (nested lists)
(test-program
  '(program 
    ((simple-stament 
      (var-declaration 
        (var-assign 
          (var-type-name (list-t) "innerList") 
          (exp6 (exp5 (exp4 (exp3 (exp2 (exp1 (exp0 (atom (value-atom (lst-val ((int-val 1) (int-val 2)))))))))))))))
     (simple-stament 
      (var-declaration 
        (var-assign 
          (var-type-name (list-t) "outerList") 
          (exp6 (exp5 (exp4 (exp3 (exp2 (exp1 (exp0 (atom (value-atom (lst-val ((str-val "hello")))))))))))))))
     (simple-stament 
      (expression 
        (exp6 (exp5 (exp4 (exp3 (exp2 (exp1 (exp0 (atom (predefined-atom (push-statement "outerList" (exp6 (exp5 (exp4 (exp3 (exp2 (exp1 (exp0 (atom (var-name-atom "innerList")))))))))))))))))))))
     (simple-stament 
      (expression 
        (exp6 (exp5 (exp4 (exp3 (exp2 (exp1 (exp0 (atom (predefined-atom (print-statement (exp6 (exp5 (exp4 (exp3 (exp2 (exp1 (exp0 (atom (var-name-atom "outerList")))))))))))))))))))))))
  "Push list into another list - nested lists")

(display "=== All List Tests Complete ===")
(newline)
