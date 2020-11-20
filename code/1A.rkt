#lang racket

(define A (* 5 5))

(define B (+ A (* 5 A)))

(define square (lambda (x)(* x x)))

(define (average x y)
  (/ (+ x y) 2))

(define (mean-square x y)
  (average (square x) (square y)))

(define (abs x)
  (cond 	((< x 0) (- x))
                ((= x 0) 0)
                ((> x 0) x)))


(define (try guess x)
  (if (good-enough? guess x)
      guess
      (try (improve guess x) x)))

(define (sqrt x) (try 1 x))

(define (improve guess x)
  (average guess (/ x guess)))

(define (good-enough? guess x)
  (< (abs (- (square guess) x))
     .01))
