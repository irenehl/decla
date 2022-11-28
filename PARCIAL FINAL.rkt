#lang racket
;ALEXANDER ERNESTO MENENDEZ NAVARRETE 00098818

;EJERCICIO 1
;(espacio 3 2 1)
(define (espacio x y z)
  (sqr (+ (+ (sqr(- 3 x)) (sqr (- 2 y))) (sqr (- 1 z))))
)

;(verificar '(3 2 1))
(define (verificar l)
  (<= (espacio (list-ref l 0) (list-ref l 1) (list-ref l 2))3)
)


;EJERCICIO 2

(define (guardar L)
  ; Apertura:
  (define datos-de-lista (open-output-file "coordenadas.txt" #:mode 'text #:exists 'append))
  ; Procesamiento:
  (guardar-rec L datos-de-lista)
  ; Cierre:
  (close-output-port datos-de-lista)
)

(define (guardar-rec L flujo)
  (if (not (empty? L))
      (begin
        (display (first L) flujo)
        (display "\n" flujo)
        (guardar-rec (rest L) flujo))
      (void)))

;EJERCICIO 3

(define (ejercicio3 R L)
  (if (not (null? R))
      (begin
        (append L (list (ejercicio3 (third R) L) (second R) (ejercicio3 (first R) L)))
      )
      null
  )
)