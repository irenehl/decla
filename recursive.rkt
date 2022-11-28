#lang racket

; Factorial
(define (factorial n)
  (if (> n 1)
      (* n (factorial(- n 1)))
      1
  )
)

; Contar de hasta 10
(define (contar10)
  (contarRango 1 10)
 )

(define (contarRango i n)
  (if (<= i n)
      (begin
        (display i)
        (newline)
        (contarRango (add1 i) n)
       )
      (void)
  )
)

;Sumar todos los numeros entre a y b
(define (contar a b)
  (if (< a b)
      (+ a (contar(add1 a) b))
      b
  )
)

; Sumar hasta 10
(define (sumarhasta10)
  (contar 1 10)
)

; Sumar 1 a n
(define (sumarhastaN n)
  (contar 1 n)
 )


; Divisores
(define (divisor N D)
  (if (>= D 2)
      (begin
        (if (equal? (remainder N D) 0)
            (begin
              (display D)
              (newline))
            (void))
        (divisor N (sub1 D)))
      (void)))
  