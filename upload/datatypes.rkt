#lang racket

(require (lib "eopl.ss" "eopl"))

(define (report-expval-extractor-error! type) 
  (eopl:error 'invalid-value "invalid value cast to ~s" type))


(define-datatype environment environment?
  (empty-environment)
  (extend-environment (var string?) (val expval?) (env environment?)))


(define expval->num
  (lambda (val) 
    (cases expval val
      (num-val (num) num)
      (else (report-expval-extractor-error! "number")))))

(define expval->bool
  (lambda (val) 
    (cases expval val
      (bool-val (bool) bool)
      (else (report-expval-extractor-error! "boolean")))))

(define expval->string
  (lambda (val) 
    (cases expval val
      (string-val (str) str)
      (else (report-expval-extractor-error! "string")))))

(define expval->char
  (lambda (val) 
    (cases expval val
      (char-val (ch) ch)
      (else (report-expval-extractor-error! "char")))))

(define expval->float
  (lambda (val) 
    (cases expval val
      (float-val (fl) fl)
      (else (report-expval-extractor-error! "float")))))

(define expval->function
  (lambda (val) 
    (cases expval val
      (function-val (params body closure-env) (list params body closure-env))
      (else (report-expval-extractor-error! "function")))))

(define expval->return
  (lambda (val) 
    (cases expval val
      (return-val (ret-val) ret-val)
      (else (report-expval-extractor-error! "return")))))

(define expval->list
  (lambda (val) 
    (cases expval val
      (list-val (lst) lst)
      (else (report-expval-extractor-error! "list")))))


(define expval->string-for-print
  (lambda (val)
    (cases expval val
      (num-val (num) (number->string num))
      (float-val (fl) (number->string fl))
      (bool-val (bool) (if bool "true" "false"))
      (string-val (str) 
        
        (let ((clean-str (if (and (> (string-length str) 1)
                                 (eq? (string-ref str 0) #\")
                                 (eq? (string-ref str (- (string-length str) 1)) #\"))
                            (substring str 1 (- (string-length str) 1))
                            str)))
          clean-str))
      (char-val (ch) 
        
        (let ((clean-ch (if (and (> (string-length ch) 1)
                                (eq? (string-ref ch 0) #\')
                                (eq? (string-ref ch (- (string-length ch) 1)) #\'))
                           (substring ch 1 (- (string-length ch) 1))
                           ch)))
          clean-ch))
      (list-val (lst) 
        (string-append "[" 
          (string-join (map expval->string-for-print lst) ", ") 
          "]"))
      [else "unknown"])))


(define string-join
  (lambda (str-list separator)
    (cond
      [(null? str-list) ""]
      [(null? (cdr str-list)) (car str-list)]
      [else (string-append (car str-list) separator (string-join (cdr str-list) separator))])))


(define-datatype expval expval?
  (num-val (num number?))
  (float-val (fl number?))
  (bool-val (bool boolean?))
  (string-val (str string?))
  (char-val (ch string?))
  (break-val)
  (continue-val)
  (return-val (val expval?))
  (function-val (params list?) (body list?) (closure-env environment?))
  (list-val (lst list?)))

(provide (all-defined-out))