#lang racket

(define (atom? x)
  (and (not (null? x))
       (not (pair? x))))

(define (deriv exp var)
  (cond
    ((CONST? exp var) 0)
    ((SAME-VAR? exp var) 1)
    ((SUM? exp)
     (make-sum
      (deriv (A1 exp) var)
      (deriv (A2 exp) var)))
    ((PRODUCT? exp)
     (make-sum
      (make-product
       (M1 exp)
       (deriv (M2 exp) var))
      (make-product
       (deriv (M1 exp) var)
       (M2 exp))))))

(define (CONST? exp var)
  (and
   (atom? exp)
   (not (eq? exp var)))
  )

(define (SAME-VAR? exp var)
  (and
   (atom? exp)
   (eq? exp var))
  )


(define (SUM? exp)
  ; An expression is a sum if its first element equals '+
  (and
   (not (atom? exp))
   (eq? (car exp) '+)) ; notice the quotation.
  )

;(define (make-sum a1 a2)
;  (list '+ a1 a2))

(define (PRODUCT? exp)
  (and
   (not (atom? exp))
   (eq? (car exp) '*)))

(define (make-product m1 m2)
  (list '* m1 m2))

; CADR is the car of the CDR (second element of exp)
; CADDR is the car of the CDR of the CDR (third element of exp)

(define A1 cadr)	
(define A2 caddr)

(define M1 cadr) 	
(define M2 caddr)

(define foo					; a*x*x + b*x + c
  '(+	(* a (* x x))
        (+ (* b x)
           c)))



(define (make-sum a1 a2)
  (cond
    ((and
      (number? a1)
      (number? a2))
     (+ a1 a2))
    ((and
      (number? a1)
      (= a1 0))
     a2)
    ((and
      (number? a2)
      (= a2 0))
     a1)
    (else (list '+ a1 a2)))
  )




  