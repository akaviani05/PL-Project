#lang racket

(require "interpreter.rkt")

; Test basic arithmetic operations
(displayln "Testing basic arithmetic operations:")
(displayln "5 + 3 =")
(displayln (test-add 5 3))
(displayln "10 - 3 =")
(displayln (test-sub 10 3))

; Test complex expressions
(displayln "\nTesting complex expression: 5 + (10 - 3) =")
(displayln (test-complex))

; Test variables
(displayln "\nTesting variables: x=5, y=3, x+y =")
(displayln (test-variables))

; Test negative numbers
(displayln "\nTesting with negative result: 3 - 10 =")
(displayln (test-sub 3 10))
