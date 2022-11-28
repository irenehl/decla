#lang racket

;Alexander Ernesto Menéndez Navarrete 00098818
;Miguel Angel Mesquita Portillo 00043818
;Walter Rafael Morales Henríquez 00019618


;EJERCICIO 1
(display "::Ejercicio 1::\n")
(define (cadena cadenita)
  (begin
    (display "Su cadena es: \n")
    (display cadenita)
    )
  )
(display "Ingrese la Cadena:\n")   
(cadena (read-line))


;EJERCICIO 2
(display "\n\n::Ejercicio 2 ::\n")
(display "Ingrese la Cadena a Invertir:\n")
(define ca1 (read-line))
(define (invierte cadena)
  (list->string (reverse (string->list cadena)))
)
(invierte ca1)

;EJERCICIO 3
(define lista1 (list 1 2 3))
(define lista2 (list 4 5 6))
(define lista3 (list 7 8 9))

(display "\n\n::Ejercicio 3::\n")
(define (ejercicio3 list1 list2 list3)
    (cond [(empty? list1) null]
        [else (append (list (first list1)) (list (first list2)) (list (first list3)) (ejercicio3 (rest list1) (rest list2) (rest list3)))]
    )
)
 
(ejercicio3 lista1 lista2 lista3)

;EJERCICIO 4

(define list1 (list 10 11 12))
(define list2 (list 13 14 15))
(define list3 (list 16 17 18))

(display "\n\n::Ejercicio 4::\n")
(define (ejercicio4 list1 list2 list3)
    (cond [(empty? list1) null]
        [else (append (list (list (first list1) (first list2) (first list3))) (ejercicio4 (rest list1) (rest list2) (rest list3)))]
    )
)
 
(ejercicio4 list1 list2 list3)


; EJERCICIO 5

(define (L1 parametros)
(display "\n\n::Ejercicio 5::\n") 
(L2 parametros '() '() '() '() )
)
(define (L2 parametros numeros cadena simbolo caracteres)
(if (not (empty? parametros))
   (begin
     (cond
     ((number? (first parametros)) (L2  (rest parametros) (append  (list (first parametros)) numeros) cadena simbolo caracteres ) ) 
     ((string? (first parametros)) (L2  (rest parametros) numeros (append  (list (first parametros)) cadena) simbolo caracteres ))
     ((symbol? (first parametros)) (L2  (rest parametros) numeros cadena (append  (list (first parametros)) simbolo) caracteres ))
     ((char? (first parametros)) (L2  (rest parametros) numeros cadena simbolo (append  (list (first parametros)) caracteres) ))
     (L2  (rest parametros) numeros cadena simbolo caracteres )
     )
    )
   (values numeros cadena simbolo caracteres)
 )
)