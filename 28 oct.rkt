#lang racket
;if de dos bloques:
(define(prueba-if x)
  (if (> x 0)
      (display "si")
      (display "no")))

;un if mas siemple:
(define (if-simple)
  (if 5
      7
      3))

;dos numeros mayores que 10
(define(comparar a b)
  (if (> (+ a b) 10)
      (display "La suma es mayor que 10")
      (display "La suma es menor que 10")))

;dos numeros mayores, menor , igual que 10
(define(comparar2 a b)
  (if (> (+ a b) 10)
      (display "La suma es mayor que 10")
      (if (< (+ a b) 10)
          (display "La suma es menor que 10")
          (display "La suma es igual que 10"))))

;Dias de la semana
(define (dias dia)
  (cond
    ((equal? dia 1) (display "Lunes"))
    ((equal? dia 2) (display "Martes"))
    ((equal? dia 3) (display "Miercoles"))
    ((equal? dia 4) (display "Jueves"))
    ((equal? dia 5) (display "Viernes"))
    ((equal? dia 6) (display "Sabado"))
    ((equal? dia 7) (display "Domingo"))
    (else (display "Numero de dia no valido"))
    ))


;Dias de la semana utilizando case
(define (dias2 dia)
  (case dia
    ((1) (display "Lunes"))
    ((2) (display "Martes"))
    ((3) (display "Miercoles"))
    ((4) (display "Jueves"))
    ((5) (display "Viernes"))
    ((6) (display "Sabado"))
    ((7) (display "Domingo"))
    (else (display "Numero de dia no valido"))
    ))
;leer valores
(define (leerValor)
(read)
 )

(define(mostrarValorLeido a)
  (display "El valor leido es: ")
  (display a))


(define (hacer-funcion funcion x)
  (funcion x)
  )

(define (sumar10 x)
  (+ x 10)
  )
