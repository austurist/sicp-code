#lang racket

(define (+rat x y)
  (make-rat
   (+  (* (numer x) (denom y))
       (* (denom x) (numer y)))
   (* (denom x) (denom y))))

(define (*rat x y)
  (make-rat
   (* (numer x) (numer y))
   (* (denom x) (denom y))))

(define (make-rat n d)
  (let ((g (gcd n d)))
    (cons (/ n g) (/ d g))))

(define (numer x) (car x))

(define (denom x) (cdr x))

; segments

(define (make-vector x y) (cons x y))

(define (xcor p) (car p))

(define (ycor p) (cdr p))


(define (make-seg p q) (cons p q))

(define (seg-start s) (car s))

(define (seg-end s) (cdr s))

(define average (λ (x y) (/ (+ x y) 2)))

(define (midpoint s)
  (let 	((a (seg-start s))
         (b (seg-end s)))
    (make-vector
     (average (xcor a) (xcor b))
     (average (ycor a) (ycor b)))))

; define pairs

(define (cons a b)
  (lambda (pick)
    (cond ((= pick 1) a)
          ((= pick 2) b))))

(define (car x) (x 1))

(define (cdr x) (x 2))
