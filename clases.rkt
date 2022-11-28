#lang racket
;suma de dos numeros:
(define(suma a b)
  (+ a b)
  )

;Longitud de una circunferencia:
(define(longitud-circunferencia r)
  (* 2(* pi r))
  )

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


(define (raizCuadratica a b c)
(/ (+ (- b) (sqrt (- (sqr b) (* 4 a c)))) (* 2 a))
(/ (+ (- b) (sqrt (- (sqr b) (* 4 a c)))) (* 2 a))
)



(define (raizCuadratica2 a b c)
(display (/ (+ (- b) (sqrt (- (sqr b) (* 4 a c)))) (* 2 a)))
(display "\n")
(display (/ (- (- b) (sqrt (- (sqr b) (* 4 a c)))) (* 2 a)))
)


(define (raizCuadratica3 a b c)
(list
(/ (+ (- b) (sqrt (- (sqr b) (* 4 a c)))) (* 2 a))
(/ (- (- b) (sqrt (- (sqr b) (* 4 a c)))) (* 2 a))
)
)

;CLASE 2
(define (prueba-if x)
  (if (> x 0)
      (display "si")
      (display "si"))
  
  )

;1) Un simple if.
(define (pruebaif)
(if 5
7
3
))

; 2) Escribir una función que nos indique si el valor que recibe es positivo o negativo.
(define (signoDeX x)
(if (positive? x)
(display "El número es positivo")
(display "El número es negativo")))


;3) Escriba una función que indique si la suma de dos números es mayor o menor
;que 10.
(define (compararCon10 a b)
(if (> (+ a b) 10)
(display "La suma es mayor que 10")

(display "La suma es menor que 10")))

 ;4) Escriba una función que indique si la suma de dos números es mayor, menor
;que 10 o igual a 10.

(define (comparar-suma a b)
(if (> (+ a b) 10)
(display "La suma es mayor que 10")
(if(< (+ a b) 10)
(display "La suma es menor que 10")
(display "La suma es igual a 10"))))

(define (comparar-suma2 a b)
(if(< (+ a b) 10)
(begin
(display "La suma ")
(display "es menor que 10"))
(begin
(display "La suma ")
(display "es mayor que 10"))))

;CLASE 3
; Facorial:
(define (factorial n)
  (if (> n 1)
      (* n (factorial (- n 1)))
      1
  )
)

; Contar hasta 10:
(define (imprimirHasta10)
  (imprimirHasta 1 10))

(define (imprimirHasta i n)
  (if (<= i n)
      (begin
        (display i)
        ;(display "\n")
        (newline)
        (imprimirHasta (add1 i) n))
      (void)))


; Calcular y retornar la suma de los número del 1 al 10:
(define (sumar_1_a_10)
  (acumular 1 10))

(define (acumular i n)
  (if (< i n)
      (+ i (acumular (add1 i) n)) 
      n))


; Calcular y retornar la suma de los número del 1 a n:
(define (sumar_1_a_N N)
  (acumular 1 N))


; Calcular la suma de los números en el
; intervalo [a, b]:
(define (sumar_de_A_a_B A B)
  (acumular A B))


; Desplegar los divisores de un número entero positivo:
(define (divisoresDeN N)
  (if (and (integer? N) (positive? N))
      (divisor N (quotient N 2))
      (display "El N introducido no es entero o positivo")))

(define (divisor N D)
  (if (>= D 2)
      (begin
        (if (equal? (remainder N D) 0)
            (begin
              (display D)
              (newline))
            (void))
            (divisor N (sub1 D)))
      (display 1)))


;CLASE 5
; Queremos invocar así:
; > (recorrer-arbol (list (list (list null 3 null) 5 (list null 8 null)) 10 (list (list null 12 null) 15 (list null 20 null))))
(define (recorrer-arbol A)
  (if (not (equal? A null))
      (begin
        (recorrer-arbol (first A))
        (display (second A))
        (newline)
        (recorrer-arbol (third A))
      )
      (void)
  )
)


; > (buscar-dato-en-arbol (list (list (list null 3 null) 5 (list null 8 null)) 10 (list (list null 12 null) 15 (list null 20 null))) 15)

(define (buscar-dato-en-arbol A dato)
  (if (not (equal? A null))
      (if (< dato (second A))
          (buscar-dato-en-arbol (first A) dato)
          (if (> dato (second A))
              (buscar-dato-en-arbol (third A) dato)
              (display "El dato se encuentra en el árbol\n")
          )
      )
      (display "El dato no se encuentra en el árbol\n")
  )
)

(define (crear-lista-rama-busqueda A dato)
  (crear-lista A dato null)
)

(define (crear-lista A dato L)
  (if (not (null? A))
      (if (< dato (second A))
          (crear-lista (first A) dato (append L (list (second A))))
          (if (> dato (second A))
              (crear-lista (third A) dato (append L (list (second A))))
              (append L (list (second A)))
          )
      )
      L
  )
)


(define (insertar-nodo-en-arbol A dato)
  (ins-nodo A dato)
)

(define (ins-nodo A dato)
  (if (equal? A null)
      (list null dato null)
      (if (equal? (second A) dato)
          A
          (if (< dato (second A))
              (list (ins-nodo (first A) dato) (second A) (third A))
              (list (first A) (second A) (ins-nodo (third A) dato))
          )
      )
  )
)

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
        (display " " flujo)
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



;derivada de:

;una constante:                      n
;de x:                               'x
;de una constante por x:             '(* n x)
;de una constante por una función:   '(* n x)
;de x a la n:                        '(expt x n)

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


; Hacer: la derivada de la suma de dos funciones, de la resta de
; dos funciones, del producto y del cociente


(define x 10.0)

(define f '(- (expt x 2) 4))

;Método de Newton-Raphson para converger a la raíz de una función:
(define (N-R)
  (let
      (
       (xSig (- x (/ (eval f) (eval (deri 'x f))))) 
      )
      (if (>= (abs (- xSig x)) 0.0001)
          (begin
            (display xSig)
            (newline)
            (set! x xSig)
            (N-R)
          )
          xSig
      )
  )
)


(define (ecCircunferencia h k a b)
  (let
      (
       (rCuad (+ (sqr (- a h)) (sqr (- b k))))
      )
      (list '= (list '+ (list 'sqr (list '- 'x h)) (list 'sqr (list '- 'y k))) rCuad)
  )
)


(define (ecuacionRecta x1 y1 x2 y2)
  (let*
      (
       (m (/ (- y2 y1) (- x2 x1)))
       (b (- y1 (* m x1)))
      )
      (list '+ (list '* (eval m) 'x) (eval b))
  )
)


;;;;;;;;; Integrales ;;;;;;;;;;
(define (integralEjm1 x expresion)
  (if (number? expresion)
      (list '* expresion 'x)
      (void) ;Pendiente para más integrales
  )
)


(define (integralEjm2 x Expresion)
  (if (equal? Expresion 'x)
      (list '* 1/2 '(sqr x))
      (void) ;Pendiente para más integrales
  )
)


(define (integralEjm3 x Expresion)
  (if (number? Expresion)
       (list '* Expresion 'x)
      (if (equal? Expresion 'x)
          (list '* 1/2 '(sqr'x))
          'Pendiente_de_construir_más_integrales
      )
  )
)



(define (integral x Expresion)
  (if (number? Expresion)
      (list '* Expresion 'x)
      (if (equal? Expresion 'x)
          (list '* 1/2 (list 'sqr x))
          (let
              (
               (u (second Expresion))
               (v (third Expresion))
              )
              (case (first Expresion)
                ((*)
                 (if (and (number? u) (equal? v x))
                     (list '* u 1/2 (list 'sqr x))
                     (void)
                 ))
              )
          )
      )
  )
)


(define (evaluar-integral var expresion a b)
  (let*
      (
       ;(x 0) ; Ya está definida en este archivo más arriba.
       (I (integral var expresion))
       (Ia 0)
       (Ib 0)
      )
      (set! x a)
      (set! Ia (eval I))
      (set! x b)
      (set! Ib (eval I))
      (- Ib Ia)
  )
)


