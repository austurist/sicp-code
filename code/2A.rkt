#lang racket


(define (sum-int a b)
  (if (> a b)
      0
      (+ a (sum-int (add1 a) b))))

(define (sum-sq a b)
  (if (> a b)
      0
      (+ (sqr a) (sum-sq (add1 a) b))))


(define (pi-sum a b)
  (if (> a b)
      0
      (+ (/ 1 (* a (+ a 2)))
         (pi-sum (+ a 4) b))))


(define (sum term a next b)
  (if (> a b)
      0
      (+ (term a)
         (sum term (next a) next b))))


(define (sumi term a next b)
  (define (iter j ans)
    (if (> j b)
        ans
        (iter 	(next j)
                (+ (term j) ans))))
  (iter a 0)) 

; fixed point
(define (fixed-point f start)
  (define (iter old new)
    (if (close-enuf? old new)
        new
        (iter new (f new))))
  (iter start (f start)))

(define (close-enuf? old new)
  (< (abs (- old new)) 0.01))

(define (average a b) (/ (+ a b) 2.0))

(define (sqrt x)
  (fixed-point
   (λ(y) (average (/ x y) y))
   1))


; damping

(define (sqrt-ad x)
  (fixed-point
   (average-damp (λ(y)(/ x y)))
   1))

(define (average-damp f)
  (λ(x) (average (f x) x)))

; newton's method


(define (sqrt-nm x)
  (newton (λ(y)(- x (sqr y)))
          1))


(define (newton f guess)
  (define df (deriv f))
  (fixed-point (λ(x)(- x (/ (f x)(df x))))
               guess))


(define deriv
  (λ (f)
    (λ (x)
      (/ (- (f (+ x dx))
            (f x))
         dx))))

(define dx 0.0001)