#lang racket
; Calcule el volumen de un cilindro: pi*r2*h
(define (volCilindro r h)
  (* pi(expt r 2) h)
 )

; Defina si un numero X es múltiplo de 3 y 5. Su función retornará un valor booleano.
(define (multiplo n)
  (and (= (remainder n 5) 0) (= (remainder n 3) 0))
 )

 ; Valor absoluto de un número.
(define (valorAbs n)
  (display (abs n))
 )


; Muestre cuantas unidades, decenas, centenas y millares posee un número.
(define (separarN n)
  (cond
    ((< n 10) (begin
               (display "Unidades: ")
               (display n)
               (newline)
               )
        )
     ((and (> n 10) (< n 100)) (begin
                                 (display "Decenas: ")
                                 (display (quotient n 10))
                                 (newline)
                                 (separarN (remainder n 10))
                                 )
     )
     ((and(> n 100)(< n 1000)) (begin
                                 (display "Centenas: ")
                                 (display (quotient n 100))
                                 (newline)
                                 (separarN (remainder n 100))
                                 )
                               )
     ((and (> n 1000)(< n 10000))(begin
                                   (display "Millares: ")
                                   (display (quotient n 1000))
                                   (newline)
                                   (separarN (remainder n 1000))
                                   )
                                 )
     )
    )

; Elabore una función que reciba como parámetros dos números a y b. 
; Con estos números realice las cuatro operaciones básicas (suma, resta, 
; multiplicación y división) mostrando el resultado de cada operación.

(define(calculadora a b)
  (display "Suma: ")
  (display(+ a b))
  (newline)
  (display "Resta: ")
  (display(- a b))
  (newline)
  (display "Division: ")
  (display(/ a b))
  (newline)
  (display "Multiplicacion: ")
  (display(* a b))
  (newline)
)
       
; Dado dos números enteros positivos hacer un menú como el siguiente:
; a) Suma
; b) Resta
; c) Multiplicación
; d) División
; Recuerde hacer las validaciones correspondientes, por ejemplo, para la 
; división.
; Cada operación estará en una función distinta. Su función principal debe 
; tener esta forma:
; (define (menú_operaciones numero1 numero2))
; Esta función contendrá el menú desplegable.

(define (suma a b)
  (+ a b)
 )

(define (resta a b)
  (- a b)
 )

(define (multiplicacion a b)
  (* a b)
 )

(define (division a b)
  (if (= b 0)
      (display "No se puede dividir entre 0")
      (begin
        (display "Cociente: ")
        (display(/ a b))
       )
   )
)

(define(menu)
  (display "a. Suma")
  (newline)
  (display "b. Resta")
  (newline)
  (display "c. Multiplicacion")
  (newline)
  (display "d. Division")
  (newline)
)

(define(menu_operaciones num1 num2)
  (menu)
  (
   case (read)
    ((a) (display "Suma: ")(display (suma num1 num2)))
    ((b) (display "Resta: ")(display (resta num1 num2)))
    ((c) (multiplicacion num1 num2))
    ((d) (division num1 num2))
   )
)
    
  
; Pepito ha ahorrado por 5 años en diferentes alcancías. Se dio cuenta que 
; ya pesan bastante y necesita saber de cuánto dinero dispone para 
; comprarle un regalo a su mamá. Él ha recolectado:
; 679 monedas de a 1 centavo.
; 579 monedas de a 5 centavos.
; 978 monedas de a 10 centavos.
; 402 coras.
; 100 monedas de a 1 dólar.
; Elabore un programa que le ayude a Pepito a saber de cuánto dinero 
; dispone para el regalo. Él desea saber cuánto dinero tiene en total y 
; cuánto dinero tiene por cada denominación.

; Total del ejemplo : $334.04


(define(cantidadDinero monedas valor)
  (* monedas valor)
 )

(define (cuantoTengo oneCtv fiveCtv tenCtv quarter dolar)
  (let*
      (
       (centavitos (cantidadDinero oneCtv 0.01))
       (cincoCtv (cantidadDinero fiveCtv 0.05))
       (dieCtv (cantidadDinero tenCtv 0.10))
       (coras (cantidadDinero quarter 0.25))
       (dolares (cantidadDinero dolar 1))
      )
    (display "Centavos: " )
    (display centavitos)
    (newline)
    (display "Cinco ctvs: ")
    (display cincoCtv)
    (newline)
    (display "Diez ctvs: ")
    (display dieCtv)
    (newline)
    (display "Coras: ")
    (display coras)
    (newline)
    (display "Dolares: ")
    (display dolares)
    (newline)
    (display "Total: ")
    (display (+ centavitos cincoCtv dieCtv coras dolares))
    (newline)
   )
 )


; Suponga que el banco paga 3.5% para depósitos menores o iguales a 
; $40,000, 4% para depósitos menores o iguales a $260,000 y 4.5% para 
; depósitos mayores a $260,000. Escriba una función que reciba un 
; depósito y devuelva el interés asignado al depósito.

(define(calcularInteres n  porcentaje)
  (/ n ( / 100 porcentaje))
 )

(define(interes n)
  (if (number? n)
      (cond
        ((<= n 40000) (display (calcularInteres n 3.5)))
        ((<= n 260000) (display (calcularInteres n 4)))
        ((<= n 260000) (display (calcularInteres n 4.5)))
       )
      (display "Ingrese monto: ")
     )
  )



; Pepito está trabajando en “Don Pedro venta de sorbete” y necesita un 
; programa que le ayude con su problema el cual es el siguiente: 
; - Si le compran hasta 10 galones de sorbete tendrá que entregar la 
; cantidad exacta de galones.
; - Si compran hasta 40 galones de sorbete tendrá que entregar la 
; cantidad pedida más 4 galones.
; - Si le compran hasta 100 galones de sorbete tendrá que entregar 
; la cantidad pedida más 8 galones.
; - Si le compran más de 100 galones de sorbete tendrá que entregar 
; la cantidad pedida más el 10% de la cantidad solicitada.
; Recuerde, Pepito no sabe mucho de programas y matemáticas así que él 
; desea ingresar unicamente la cantidad a comprar y que le devuelva la 
; cantidad que él debe entregar al cliente (la que el cliente pidió más el 
; extra, si aplica).

(define (despacharSorbete n)
  (if (integer? n)
      (begin
        (display "Cantidad a despachar: ")
        (cond
         ((<= n 10) (display n))
         ((<= n 40) (display (+ n 4)))
         ((<= n 100) (display (+ n 8)))
         ((> n 100) (display (+ n (* n 0.1))))
        )
      )
      (display "Ingresa un numero valido")
  )
)






































