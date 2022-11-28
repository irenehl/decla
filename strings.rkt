#lang racket

;Borrar caracter de una palabra por posicion
(define (borrarChar str pos)
  (let*
      (
       (substr1 (substring str 0 pos))
       (substr2 (substring str (+ pos 1)(string-length str)))
       (substr3 (string-append substr1 substr2))
       )
        (display substr3)
       
  )
)

; Imprimir cadena letra por letra
(define (mostrarCadena str)
  (imprimirCadena str 0 (string-length str))
)

(define (imprimirCadena str i n)
  (if (< i n)
      (begin
        (display (string-ref str i))
        (newline)
        (imprimirCadena str (add1 i) n)
      )
      (newline)
  )
)

; Contar cuantas veces se repite un caracter
(define (contarRepeticiones str char)
  (obtenerOcurrencias str 0 char 0)
 )

(define (obtenerOcurrencias str i char cont)
  (cond
    ((equal? i (string-length str)) cont)
    ((not (char=? char (string-ref str i))) (obtenerOcurrencias str (add1 i) char cont))
    ((char=? char (string-ref str i)) (obtenerOcurrencias str (add1 i) char (add1 cont)))
  )
)