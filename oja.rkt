#lang racket
;----------------------------------------------------------------------------------------------------------------------------------------------------------------;
;------------------------------------------------------------------------ INTEGRALES ----------------------------------------------------------------------------;
;----------------------------------------------------------------------------------------------------------------------------------------------------------------;

; Integral de una función de una variable:
(define (integral x expresion)
  (cond
    ; Integral de una constante:
    ((number? expresion) (list '* expresion x))
    ; Integral de la variable respecto a sí misma:
    ((equal? expresion x) (list '* (/ 1 2) (list 'expt x 2)))
    (else
     (let
         (
           (operador (first expresion))
           (u (second expresion))
           (v (third expresion))
          )
       (case operador
         ((expt)
          (cond
            ; x elevada a un exponente numérico:
            ((and (equal? u x) (number? v)) (list '* (/ 1 (add1 v)) (list 'expt u (add1 v))))
            ;Una constante elevada a una función:
            ; Hacerla...
          )
         )
         ((*)
            (cond
              ; Una constante por x:
              ((and (number? u) (equal? v x)) (list '* u (integral x v)))
              ; Una constante por una función:
              ((and (number? u) (list? v)) (list '* u (integral x v)))
              ; Más cosas...
            )
         )
         ((+)
           ; Derivada de la suma de dos funcionae:
           (list '+ (integral 'x u) (integral 'x v))
         )
         ((-)
           ; Derivada de la resta de dos funcionae:
           (list '- (integral 'x u) (integral 'x v))
         )
       )
     )
    )
  )
)

;----------------------------------------------------------------------------------------------------------------------------------------------------------------;
;------------------------------------------------------------------------ ELIPSE --------------------------------------------------------------------------------;
;----------------------------------------------------------------------------------------------------------------------------------------------------------------;

; Ecuación de la elipse centrada en
; el origen, dadas las longitudes de
; sus semiejes mayor y menor:
(define (ecElipse a b)
  (list '= (list '+ (list '/ (list 'sqr 'x) (sqr a)) (list '/ (list 'sqr 'y) (sqr b))) 1)
)


; Calcular la ecuación de la elipse,
; los focos de la elipse y verificar
; si un punto se localiza en el contorno:
;(define x 0)
;(define y 0)
(define (perteneceElip a b x y)
  (let*
      (
        (ecElip (list '= (list '+ (list '/ (list 'sqr 'x) (sqr a)) (list '/ (list 'sqr 'y) (sqr b))) 1))
        (x1 (sqrt (- (sqr a) (sqr b))))
        (F1 (list x1 0))
        (x2 (- x1))
        (F2 (list x2 0))
        (d1 (sqrt (+ (sqr (- x x1)) (sqr y))))
        (d2 (sqrt (+ (sqr (- x x2)) (sqr y))))
        (D (+ d1 d2))
        (pertenece (equal? D (* 2 a)))
      )
      (values
         ecElip
         F1
         F2
         (list x y)
         pertenece
      )
  )
)


;----------------------------------------------------------------------------------------------------------------------------------------------------------------;
;--------------------------------------------------------------------------- PLANO ------------------------------------------------------------------------------;
;----------------------------------------------------------------------------------------------------------------------------------------------------------------;

; Para recordar se puede ver:
; https://www.youtube.com/watch?v=XqHLWWabnQ8

; Calcular las componentes de un
; vector en los tres ejes:
(define (calcular-vector x1 y1 z1 x2 y2 z2)
  (list (- x2 x1) (- y2 y1) (- z2 z1))
)


; Producto cruz de dos vectores:
(define (producto-cruz v1 v2)
  (list
    (- (* (second v1) (third v2)) (* (second v2) (third v1)))
    (* -1 (- (* (first v1) (third v2)) (* (first v2) (third v1))))
    (- (* (first v1) (second v2)) (* (first v2) (second v1)))
  )
)


; Obtener la ecuación del plano que
; pasa por tres puntos:
; Recibe dos listas, cada una con
;  las tras componentes de cada vector:
(define (ecuacion-plano x0 y0 z0 x1 y1 z1 x2 y2 z2)
  (local
    (
      (define Vab (calcular-vector x0 y0 z0 x1 y1 z1))
      (define Vac (calcular-vector x0 y0 z0 x2 y2 z2))
      (define pc (producto-cruz Vab Vac))
    )
    (list '=
          (list '+
                (list '* (first pc) 'x) 
                (list '* (second pc) 'y)
                (list '* (third pc) 'z)
                )
          (+ (* (first pc) x0) (* (second pc) y0) (* (third pc) z0))
          )
    )
)

; Ejemplo: Pa:(2, 2, -3), Pb:(3, 1, 7), Pc:(5, 5, 1):
;(ecuacion-plano 2 2 -3 3 1 7 5 5 1)


;----------------------------------------------------------------------------------------------------------------------------------------------------------------;
;---------------------------------------------------------------------- CIRCUNFERENCIA --------------------------------------------------------------------------;
;----------------------------------------------------------------------------------------------------------------------------------------------------------------;


; Ecuación de la circunferencia
; dado el centro y una coordenada sobre
; el perímetro:
(define (ecCircunferencia h k a b)
  (let
      (
        (r (sqrt (+ (sqr (- a h)) (sqr (- b k)))))
      )
      (list '= (list '+ (list 'sqr (list '- 'x h)) (list 'sqr (list '- 'y k))) (sqr r))
  )
)


; Verificar si un punto se localiza en
; el interior de una circunferencia:
(define x 0)
(define y 0)
(define (perteneceCirc h k a b xp yp)
  (let*
      (
        (r (sqrt (+ (sqr (- a h)) (sqr (- b k)))))
        (inecCirc (list '< (list '+ (list 'sqr (list '- 'x h)) (list 'sqr (list '- 'y k))) (sqr r)))
        (x xp)
        (y yp)
        (pertenece ( < (sqrt (+ (sqr (- xp h)) (sqr (- yp k)))) r))
      )
      (values
         inecCirc
         (list xp yp)
         pertenece
      )
  )
)

;----------------------------------------------------------------------------------------------------------------------------------------------------------------;
;-------------------------------------------------------------------------- RECTA -------------------------------------------------------------------------------;
;----------------------------------------------------------------------------------------------------------------------------------------------------------------;

(define (ecuacionRecta x1 y1 x2 y2)
  (let*
    (
      (m (/ (- y2 y1) (- x2 x1)))
      (b (- y1 (* m x1)))
    )
    (list '+ (list '* (eval m) 'x) (eval b))
  )
)

;----------------------------------------------------------------------------------------------------------------------------------------------------------------;
;------------------------------------------------------------------------- DERIVADAS ----------------------------------------------------------------------------;
;----------------------------------------------------------------------------------------------------------------------------------------------------------------;

; a) Derivada de una constante:
(define (dK x expresion)
  (if (number? expresion)
    0
    (display "No se puede resolver para esta expresión")
  )
)


; b) Derivada de x respecto a x:
(define (dX x expresion)
  (if (equal? expresion x)
    1
    (display "No se puede resolver para esta expresión")
  )
)


; c) Derivada de una constante por x:
(define (dKX x expresion)
  (if (equal? (first expresion) '*)
    (if (and (number? (second expresion)) (equal? (third expresion) 'x))
      (second expresion)
      (display "No se puede resolver para esta expresión")
    )
    (display "No se puede resolver para esta expresión")
  )
)


; d) Derivada de x a la n:
(define (dXN x expresion)
  (if (equal? (first expresion) 'expt)
    (if (and (equal? (second expresion) 'x) (number? (third expresion)))
      (list '* (third expresion) (list 'expt 'x (sub1 (third expresion))))
      (display "No aplica")
    )
    (display "No se puede resolver para esta expresión")
  )
)


; e) Derivada de a a la x:
(define (dAX x expresion)
  (if (equal? (first expresion) 'expt)
    (if (and (number? (second expresion)) (equal? (third expresion) 'x))
      (list '* (list 'expt (second expresion) 'x) (list 'log (second expresion)))
      (display "No se puede resolver para esta expresión")
    )
    (display "No se puede resolver para esta expresión")
  )
)


; Una sola función de derivada:
(define (derivada x expresion)
  (cond
    ; Derivada de una constante:
    ((number? expresion) 0)
    ; Derivada de la variable respecto a sí misma:
    ((equal? expresion x) 1)
    (else
     (let
         (
          (u (second expresion))
          (v (third expresion))
          )
       (case (first expresion)
         ((expt)
          (cond
            ; x elevada a un exponente numérico:
            ((and (equal? u x) (number? v)) (list '* v (list 'expt u (sub1 v))))
            ; Una función elevada a otra función:
            ((and (list u) (list v)) (list '* (list 'expt u v) (list '+ (list '* (derivada x v) (list 'log u)) (list '/ (list '* v (derivada x u)) u))))
            ; Un número elevado a una función:
            ((and (number? u) (list? v)) ((* (expt u v) (log u) (derivada x v))))
            ; Un número elevado a otro número:
            ((and (number? u) (number? v)) 0)
          )
         )
         ((*)
            (cond
              ; Una constante por x:
              ((and (number? u) (equal? v x)) u)
              ; Una constante por una función:
              ((and (number? u) (list? v)) (list '* u (derivada x v)))
              ; El producto de dos funciones:
              ((and (list? u) (list v)) (list '+ (list '* u (derivada x v)) (list '* v (derivada x u))))
            )
         )
       )
     )
    )
  )
)


; Convergencia a la raíz de una función
; por el método de Newton-Raphson:

(define (f)
  (list 'expt 'x 2)
)


(define (convergencia)
  (eval '(- x (/ (eval (f)) (eval (derivada 'x (f))))))
)


; Elaborar una función que aplique recursivamente
; el método de Newton-Raphson para converger
; a la raíz de una función:


;----------------------------------------------------------------------------------------------------------------------------------------------------------------;
;------------------------------------------------------------------------ ARCHIVOS ------------------------------------------------------------------------------;
;----------------------------------------------------------------------------------------------------------------------------------------------------------------;


; Utilizar solamente las instrucciones
; de apertura y cierre:
(define (crear nombre)
  (define nombre-ext (string-append nombre ".txt"))
  (define archivo (open-output-file nombre-ext
                                    #:mode 'text
                                    #:exists 'replace))
  (close-output-port archivo)
)


; Leer desde teclado hasta
; introducir cero:
(define (leer-hasta-cero)
  ; Apertura:
  (define archivo (open-output-file "datos-de-teclado.txt"
                                    #:mode 'text
                                    #:exists 'replace))
  ; Procesamiento:
  (leer-hasta-cero-rec archivo)
  ; Cierre:
  (close-output-port archivo)
)


(define (leer-hasta-cero-rec flujo)
  (local
    (
      (define dato (read))
    )
    (if (not (zero? dato))
        (begin
          (display dato flujo)
          (newline flujo)
          (leer-hasta-cero-rec flujo)
        )
        (void)
    )
  )
)


; Leer el contenido de un archivo y
; mostrarlo en pantalla:
(define (mostrar-contenido nombre-archivo)
  ; Apertura:
  (define archivo (open-input-file nombre-archivo
                                    #:mode 'text))
  ; Procesamiento:
  (mostrar-contenido-rec archivo)
  ; Cierre:
  (close-input-port archivo)
)


(define (mostrar-contenido-rec flujo)
  (local
    (
      (define linea (read-line flujo 'any))
    )
    (if (eof-object? linea)
        (void)
        (begin
          (display linea)
          (newline)
          (mostrar-contenido-rec flujo)
        )
    )
  )
)



; Grabar el contenido de una
; lista en un archivo:
(define (procesarLista fh lista)
  (if (= 0 (length lista))
      (void)
      (begin
         (display (first lista) fh)
         (display " " fh)
         (procesarLista fh (rest lista))
        )
  )
)


(define (guardarLista lista nombre)
  (let
      (
       [fh (open-output-file nombre #:mode 'text #:exists 'replace)]
       )
    (procesarLista fh lista)
    (close-output-port fh)
    )
)