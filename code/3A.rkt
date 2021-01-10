#lang racket

(define make-vector cons)
(define xcor car)
(define ycor cdr)

(define (+vect v1 v2)
	(make-vector
		(+ (xcor v1) (xcor v2))
		(+ (ycor v1) (ycor v2))))

(define (scale s v)
  (make-vector (* s (xcor v))
               (* s (ycor v))))


  (define (coord-map rect)
    (lambda (point)
      (+vect
       (+vect 	(scale 	(xcor point))
                (horiz rect))
       (scale 	(ycor point)
                (vert rect))
       (origin rect))))
  ;# END SLIDE



  ;# SLIDE 0:45:30

  (define (make-picture seglist)
    (lambda (rect)
      (for-each
       (lambda (s)
         (drawline
          ((coord-map rect) (seg-start s))
          ((coord-map rect) (seg-end s))))
       seglist)))
  ;# END SLIDE




  ;# SLIDE 0:52:32
  (define (beside p1 p2 a)
    (lambda (rect)
      (p1	(make-rect
                 (origin rect)
                 (scale a (horiz rect))
                 (vert rect)))
      (p2	(make-rect
                 (+vect 	(origin rect)
                                (scale a (horiz rect)))
                 (scale (- l a) (horiz rect))
                 (vert rect)))))
  ;# END SLIDE



  ;# SLIDE 0:54:20
  (define (rotate90 pict)
  (lambda (rect)
    (pict (make-rect
           (+vect 	(origin rect)
                        (horiz rect))
           (vert rect)
           (scale -1 (horiz rect))))))
;# END SLIDE



;# BOARD 0:58:10
(define (right-push p n a)
  (if (= n 0)
      pict
      (beside
       (right-push p (- n 1) a)
       a)))
;# END BOARD



;# BOARD 1:03:50
(define (push comb)
  (λ (pict n a)
    ((repeated
      (λ(p) (comb pict p a))
      n)
     pict)))


;# END BOARD
