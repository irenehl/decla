#lang racket
; Dado un entero que representa un año, diga si es o no bisiesto.
(define (esBisiesto anio)
  (if (integer? anio)
      (cond
        ((= (remainder anio 4) 0) (display "Es bisiesto"))
        (else (display "no es"))
        )
      (display "Ingrese anio")
      )
  )

; Dado un número entero despliegue el nombre del mes que corresponde. Si el número no es válido, también indíquelo.
(define (mes num)
  (case num
    ((1) (display "Enero"))
    ((2) (display "Febrero"))
    ((3) (display "Marzo"))
    ((4) (display "Abril"))
    ((5) (display "Mayo"))
    ((6) (display "Junio"))
    ((7) (display "Julio"))
    ((8) (display "Agosto"))
    ((9) (display "Sept"))
    ((10) (display "Oct"))
    ((11) (display "Nov"))
    ((12) (display "Dic"))
    (else (display "Error"))
    )
  )

; Dados tres valores enteros que corresponden a día, mes y año, indique si es una fecha válida. Considere años bisiestos.
(define(validDate d m a)
  (if (and (and (integer?  d) (integer? m) (integer? a) (>= d 1)(<= d 31)) (>= m 1)(<= 12))
      (case m
        ((2) (begin
               (cond
                 ((and(= (remainder a 4)0) (= d 29)) (display "fecha valida"))
                 ((and(>= d 29)) (display "fecha invalida"))
                 (else display "fecha valida")
                 )
                  
               )
             )
        )
      (display "fecha invalida")
      )
      
  )

; Dados tres enteros positivos que representan los lados de un triángulo, indique si ese es equilátero, isósceles o escaleno.
(define (triangulo a b c)
  (cond
    ((= a b c) (display "equilatero"))
    ((or (= a b) (= a c) (= b c)) (display "isosceles"))
    (else (display "escaleno"))
    )
  )

;Elabore una función que, dado los puntos (x1, y1) y (x2, y2), muestre
;un menú con las siguientes opciones:
; a. Calcular la distancia entre los puntos.
; b. Calcular la pendiente de la recta a la que pertenecen ambos puntos.

(define (menu)
  (display "a. Calcular la distancia entre los puntos.")
  (newline)
  (display " b. Calcular la pendiente de la recta a la que pertenecen ambos puntos.")
  (newline)
  )

(define (distancia x1 y1 x2 y2)
  (display (abs(sqrt(+ (expt (- x2 x1) 2) (expt (- y2 y1) 2)))))
  )

(define (pendiente x1 y1 x2 y2)
  (display (/ (- y2 y1) (- x2 x1)))
  )


(define (operaciones x1 y1 x2 y2)
  (menu)
  (
   case (read)
    ((a) (display (distancia x1 y1 x2 y2)))
    ((b) (display (pendiente x1 y1 x2 y2)))
    )
  )

   



