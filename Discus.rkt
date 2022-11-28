#lang racket
; Elabore una función que almacene en un archivo
; los datos que se leen desde teclado, uno por fila,
; hasta introducir cero.
(define (leer-hasta-cero)
  ; Apertura:
  (define datos-de-teclado (open-output-file "datos-de-teclado.txt" #:mode 'text #:exists 'replace))
  ; Procesamiento:
  (leer-hasta-cero-rec datos-de-teclado)
  ; Cierre:
  (close-output-port datos-de-teclado)
)

(define (leer-hasta-cero-rec flujo)
  (local
    (
     (define dato (read))
    )
    (if (not (equal? dato 0))
        (begin
          (display dato flujo)
          (display "\n" flujo)
          ;(newline flujo)
          (leer-hasta-cero-rec flujo)
        )
        (void)
    )
  )
)


; Elabore una función que, dada una lista de números enteros,
; almacene en un archivo los valores de la lista, uno por fila.
(define (guardar-datos-de-lista L)
  ; Apertura:
  (define datos-de-lista (open-output-file "datos-de-lista.txt" #:mode 'text #:exists 'replace))
  ; Procesamiento:
  (guardar-datos-de-lista-rec L datos-de-lista)
  ; Cierre:
  (close-output-port datos-de-lista)
)

(define (guardar-datos-de-lista-rec L flujo)
  (if (not (empty? L))
      (begin
        (display (first L) flujo)
        (display "\n" flujo)
        (guardar-datos-de-lista-rec (rest L) flujo))
      (void)))


; Elabore una función que, dada una lista de números enteros,
; almacene en un archivo los factoriales de dichos números.
(define (guardar-factoriales L)
  ; Apertura:
  (define datos-de-lista (open-output-file "factoriales-de-lista.txt" #:mode 'text #:exists 'replace))
  ; Procesamiento:
  (guardar-factoriales-rec L datos-de-lista)
  ; Cierre:
  (close-output-port datos-de-lista)
)

(define (guardar-factoriales-rec L flujo)
  (if (not (empty? L))
      (begin
        (display (factorial (first L)) flujo)
        (display " " flujo)
        (guardar-factoriales-rec (rest L) flujo))
      (void)))

(define (factorial n)
  (if (> n 1)
      (* n (factorial (- n 1)))
      1))


;Derivada de una constante:
(define (deri-cte x Expresion)
  (if (number? Expresion)
      0
      'Falta_construir_más_derivadas
  )
)

;Derivada de x respecto a x:
(define (deri-x x Expresion)
  (if (equal? Expresion x)
      1
      'Falta_construir_más_derivadas
  )
)


;Incorporando los dos casos de derivada ya realizados:
(define (deri-dos x Expresion)
  (if (number? Expresion)
      0
      (if (equal? Expresion x)
          1
          'Falta_construir_más_derivadas
      )
   )
)



; Función que contempla varias derivadas:
(define (deri x Expresion)
  (if (number? Expresion) ;Derivada de una constante.
      0
      (if (equal? Expresion x) ;Derivada de x respecto a x.
          1
          (let
              (
               (u (second Expresion))
               (v (third Expresion))
              )
              (case (first Expresion)
                ((expt) ;Derivada de x a la n.
                 (if (and (equal? u x) (number? v))
                     (list '* v (list 'expt u (- v 1)))
                     'Falta_construir_más_derivadas_con_potencia
                 ))
                ((*)
                 (if (and (number? u) (equal? v x)) ;Derivada una constante por x.
                     u
                     (if (and (number? u) (list? v)) ;Derivada una constante por una función.
                         (list '* u (deri x v))
                         (if (and (list? u) (list? v))
                             (list '+ (list '* (deri x u) v) (list '* u (deri x v)))
                             (void)
                             )
                     )))
                ((/)
                  (if (and (list? u) (list? v))
                      (list '/ (list '- (list '* (deri x u) v) (list '* u (deri x v))) (list 'sqr v))
                      (void)
                   )
                )
                ((+)
                 (if (and (list u) (list v))
                     (list '+ (deri x u) (deri x v))
                     (void)
                     ))
                ((-)
                 (if (and (list u) (list v))
                     (list '- (deri x u) (deri x v))
                     (void)
                 ))
            )
       )
    )
  )
)


; Dado un entero que representa un año, diga si es o no bisiesto.
(define (bisiesto numero)
  (if (and (= 0 (modulo numero 4)) (or (not(= 0 (modulo numero 100))) (= 0 (modulo numero 400))))
      (display "El año es bisiesto")
      (display "El año no es bisiesto")
      )
)

; Dados tres enteros positivos que representan los lados de un triángulo,
; indique si ese es equilátero, isósceles o escaleno.
(define (triangulo n1 n2 n3)
  (cond
    ( (= n1 n2 n3) (display "Triangulo Equilatero"))
    ( (or (= n1 n2) (= n1 n3) (= n2 n3)) (display "Triangulo Isosceles"))
    ( (not(= n1 n2 n3)) (display "Triangulo Escaleno"))
    )
)

;Dado un número positivo que representa el radio de una circunferencia,
;realice lo siguiente: a) si el radio es mayor que cero, calcular e indicar el
;área de la circunferencia, b) si el valor del radio es cero, indicar que se
;trata de un punto en el plano, pedir sus coordenadas e indicar la
;distancia del punto al origen.
(define (circunferencia r)
  (if (< 0 r)
      (begin
        (let ([area (* pi (expt r 2))]) (display area))
        )
      (begin
        (display "Es un punto \n")
        (let ([x1 (read)] [y1 (read)]) (display (sqrt (+ (expt x1 2) (expt y1 2)))))
        )
      )
)

; Cuadrado o Hexagono
(define (areade r n)
  (cond
    ( (= 4 n ) (let ([areaCua (* 2 (expt r 2))]) (display areaCua)))
    ( (= 6 n ) (let ([areaHex (* 6 (* (expt r 2) (/ (sqrt 3) 4)))]) (display areaHex)))
    ( else (display "No voy a hacer nada"))
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


; Imprimir cada elemento de
; una lista, línea por línea:
(define (imprimeEltosL L)
 (if (not (empty? L))
     (begin
       (display (first L))
       (newline)
       (imprimeEltosL (rest L))
     )
     (newline)
  )
)

; Clasificar los elementos de una lista
; en cuatro listas de: números, cadenas,
; símbolos y carácteres.
(define (clasificar L)
  (clasificar-rec L '() '() '() '())
)
  

(define (clasificar-rec L Num Cad Car Sim)
  (if (not (empty? L))
      (cond 
        ((number? (first L)) (clasificar-rec (rest L) (append Num (list(first L))) Cad Car Sim))
        ((string? (first L)) (clasificar-rec (rest L) Num (append Cad (list (first L))) Car Sim))
        ((char?   (first L)) (clasificar-rec (rest L) Num Cad (append Car (list (first L))) Sim))
        ((symbol? (first L)) (clasificar-rec (rest L) Num Cad Car (append Sim (list (first L)))))
      )
      (values Num Cad Car Sim)
  )
)

; Dada una cadena, retornar la lista compuesta
; por los carácteres de la cadena, colocados
; en orden inverso:
(define (cad-lista-inv cad)
  (c-l-inv cad '())
)

(define (c-l-inv cad L)
  (if (not (zero? (string-length cad)))
      (c-l-inv (substring cad 1 (string-length cad)) (append (list (string-ref cad 0)) L))
      L
   )
)

