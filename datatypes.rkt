#lang racket

(require (lib "eopl.ss" "eopl"))

(define (report-expval-extractor-error! type) 
  (eopl:error 'invalid-value "invalid value cast to ~s" type))

; Environment datatype
(define-datatype environment environment?
  (empty-environment)
  (extend-environment (var string?) (val expval?) (env environment?)))

; Expression value extractors
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

; Expression values - matching parser output
(define-datatype expval expval?
  (num-val (num number?))
  (float-val (fl number?))
  (bool-val (bool boolean?))
  (string-val (str string?))
  (char-val (ch string?))
  (break-val)
  (continue-val))

(provide (all-defined-out))