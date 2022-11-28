#lang racket
; Invierta un número entero que se recibe como argumento. Por ejemplo
; si se ingresa el número 275 la función devolverá el entero 572.

(define (invertirN n)
  (if (integer? n)
      (inversor n 0)
      (display "NaN")
     )
  )

(define(inversor n rev)
  (let*
     (
      (ultimo (remainder n 10))
      )
    (if (equal? n 0)
        rev
        (begin
          ; set! para reasignarme el valor a una variable
          (set! rev (+ (* rev 10) ultimo))
          (inversor (floor (/ n 10)) rev)
          )
        )
    )
  )

; Dados dos argumentos, un número entero y un dígito entre cero y 
; nueve, deberá retornar la cantidad de coincidencias del dígito que se 
; encuentran en el número.

(define (ocurrencias n d)
  (if(<= 0 d 9)
     (contador n d 0)
     (display "invalido")
    )
  )

(define (contador n d cont)
  (cond
    ((equal? (remainder n 10) d) (contador (quotient n 10) d (add1 cont)))
    ((<= 0 n 9) cont)
    (else (contador (quotient n 10) d cont))
    )
  )

; Programar un algoritmo recursivo que permita hacer la división por restas sucesivas.

(define (division_recur num den)
  (restas_suc num den 1)
)

(define (restas_suc num den cont)
  (let*
      (
       (resultado (- num den))
       )
    (if (equal? resultado 0)
        cont
        (restas_suc resultado den (add1 cont))
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


; Imprimir A con espacios
(define (imprimirA num)
  (imprimir num 1)
)

(define (imprimir num cont)
  (if (<= cont num)
      (begin
        (imprimirAux cont 1)
        (imprimir num (+ cont 1))
      )
      (begin
        (display " ")
      )
  )
)

(define (imprimirAux num c)
  (if (< c num)
      (begin
        (display " ")
        (imprimirAux num (+ c 1))
        )
      (begin
        (display "A\n")
      )
  )
)




















