#lang racket

; Elabore una función que reciba dos argumentos: una lista y un elemento. La
; función retornará cuantas veces se repite el elemento en la lista.
(define (contarRepetidos lista elemento)
  (if (list? lista)
      (contarElementosLista lista elemento 0)
      (display "No es una lista")
      
  )
)

(define (contarElementosLista lista elemento cont)
  (if (not (empty? lista))
      (if (equal? (first lista) elemento)
          (contarElementosLista (rest lista) elemento (add1 cont))
          (contarElementosLista (rest lista) elemento cont)
      )
      cont
  )
)

;Elabore un programa que cree una lista a partir de elementos que se leen
; desde teclado. Para finalizar la lectura introduzca -1.
(define (listaDesdeTeclado)
  (define n 0)
  (define lista empty)
  (let loop ()
    (when (not (equal? n -1))
      (print "Escriba. -1 para terminar")
      (set! n (read))
      (if (= n -1)
          (void)
          (set! lista (append lista (list n))))
      (loop)))
  lista
)

; Elabore una función que reciba una lista e imprima cada elemento de la
; misma, línea por línea.
(define (imprimirLista lista)
  (if (not (empty? lista))
      (begin
        (display (first lista))
        (newline)
        (imprimirLista (rest lista))
       )
      (void)
  )
)

; Elabore una función que retorne la suma de todos los elementos de una lista.
(define (sumarElementosLista lista)
  (sumarLista lista 0)
 )

(define (sumarLista lista cont)
  (if (not (empty? lista))
      (sumarLista (rest lista) (+ cont (first lista)))
      cont
  )
)

; Elabore una función que reciba una lista y retorne otra con los datos en orden inverso. No utilice la función predefinida reverse.
(define (invertirLista lista)
 (invList lista empty)
)

(define (invList lista auxLista)
  (if (not (empty? lista))
      (invList (rest lista) (append (list (first lista)) auxLista))
      auxLista
  )
)

; Elabore una función que, dada una cadena de caracteres, retorne una lista cuyos elementos sean los carácteres de la cadena colocados en el mismo orden.
(define (convertirCadenaLista str)
  (string-lista str empty)
)

(define (string-lista str lista)
  (if (not (zero? (string-length str)))
      ; para que el string salga al reves en la lista solo se cambia el append asi : (append (list (string-ref str 0) lista))
      (string-lista (substring str 1 (string-length str)) (append (list lista (string-ref str 0))))
      lista
  )
)

; Elabore una función que reciba tres listas de igual longitud y cree una sola lista en la que cada elemento de ella es una
; sublista integrada por los elementos de la misma posición de las tres listas proporcionadas
(define (combinarLista A B C)
   (if (and (equal? (length A) (length B)) (equal? (length B) (length C)))
      (conv-lista A B C empty)
      (display "Listas deben de ser del mismo tama;o")
  )
)

(define (conv-lista A B C lista)
  (if (not (empty? A))
      (conv-lista (rest A) (rest B) (rest C) (append lista (list (first A)) (list (first B)) (list (first C))))
      (display lista)
   )
)
  
; Elabore una función que reciba una lista y clasifique sus elementos en otras cuatro listas de números, cadenas, símbolos y caracteres.
(define (clasificarLista lista)
  (clasificar-rec lista empty empty empty empty)
 )

(define (clasificar-rec L Num Cad Car Sim)
  (if (not (empty? L))
      (cond
        ((number? (first L)) (clasificar-rec (rest L) (append Num (list(first L))) Cad Car Sim))
        ((string? (first L)) (clasificar-rec (rest L) Num (append Cad (list (first L))) Car Sim))
        ((char?   (first L)) (clasificar-rec (rest L) Num Cad (append Car (list (first L))) Sim))
        ((symbol? (first L)) (clasificar-rec (rest L) Num Cad Car (append Sim (list (first L)))))
       )
      (begin
        (display "numeros: ")
      (display Num)
      (newline)
      (display "cadenas: ")
      (display Cad)
      (newline)
      (display "simbolos ")
      (display Sim)
      (newline)
      (display "char ")
      (display Car)
      (newline)
      )
   )
) 