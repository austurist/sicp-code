#lang racket

(require racket/format)

(define (sos x y)
  (+ (sq x) (sq y)))

(define (sq x)
  (* x x))



; iteration
(define (plusi x y)
  (if (= x 0)
      y
      (plusi (sub1 x) (add1 y))))

; recursion
(define (plusr x y)
  (if (= x 0)
      y
      (add1 (plusr (sub1 x) y))))


(define (fib n)
  (if (< n 2)
      n
      (+
       (fib (- n 1))
       (fib (- n 2)))
      )
  )

(define (fibi n)
  (if (< n 2)
      n
      (fibhelper n 2 1 1)))


(define (fibhelper k n fibn fibn-1)
  (if (= k n)
      fibn
      (fibhelper k (add1 n) (+ fibn fibn-1) fibn))
  )

                 
(define (move n from to spare)
  (cond 	((= n 0) "DONE")
                (else
                 (move (sub1 n) from spare to)
                 (write-string (string-join (list "move from" (~a from) "to" (~a to) "\n")))
                 (move (sub1 n) spare to from)))
  )                