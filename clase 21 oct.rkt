#lang racket
;suma de dos numeros:
(define(suma a b)(+ a b) )

;Longitud de una circunferencia:
(define(longitud-circunferencia r) (* 2(* pi r)))

;Area de un trinagulo:
(define(area base altura) (/ (* base altura)2))

;Distancia entre puntos:
(define(distancia x1 y1 x2 y2) (sqrt (+ (sqrt(- x2 x1)) (sqrt(- y2 x1)))))

;Cuadrado perfecto
(define (cuadrado n)
  (integer? (sqrt n))
  )

;Problema 5a:
(define(5a x)
  (and(> x 3) (<= x 7))
 )

(define (ejemplo)
  (+ 3 5)
  (* 3 5)
  (/ 3 5)
  )