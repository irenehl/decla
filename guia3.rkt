#lang racket

; Invierta un número entero que se recibe como argumento. Por ejemplo si se ingresa el número 275 la función devolverá el entero 572.
(define (invertirNumero n)
  (if (integer? n)
      (inversorNum n 0)
      (display "NaN")
  )
)

(define (inversorNum n rev)
  (let*
      (
       (ultimoDigito (remainder n 10))
       )
    (if (equal? n 0)
        rev
        (begin
          ; set! es para reasignarle el valor a una variable
          (set! rev (+ (* rev 10) ultimoDigito))
          (inversorNum (floor (/ n 10)) rev)
        )
    )
  )
)

; Dados dos argumentos, un número entero y un dígito entre cero y 
; nueve, deberá retornar la cantidad de coincidencias del dígito que se 
; encuentran en el número.

(define (contarNumOcurrencias n d)
  (if (<= 0 d 9)
      (contarDigitos n d 0)
      (display "Digito a buscar invalido")
  )
)

(define (contarDigitos n d cont)
  (cond
    ((equal? (remainder n 10) d) (contarDigitos (quotient n 10) d (add1 cont)))
    ((<= 0 n 9) cont)
    ( else (contarDigitos (quotient n 10) d cont))
  )
)

; Programar un algoritmo recursivo que permita hacer la división por restas sucesivas.
(define (division_recursiva num den)
  (restas_sucesivas num den 1)
)

(define (restas_sucesivas num den cont)
  (let*
      (
       (resultado (- num den))
       )
    (if (equal? resultado 0)
        cont
        (restas_sucesivas resultado den (add1 cont))
    )
  )
)


; Triangulo
(define (triangulo i j)
  (let*
      (
       (start i)
      )
    (if (<= start j)
         (begin
           ; make-string hace un string del tamanio que se le pase en el primer parametro, el segundo es de lo que queres que ten
           (display (make-string start #\A))
           (newline)
           (triangulo (add1 start) j)
         )
         (newline)
    )
  )
)


