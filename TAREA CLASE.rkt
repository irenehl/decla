#lang racket

;ALEXANDER ERNESTO MENENDEZ NAVARRETE 00098818

;EJERCICIO 1 CANTIDAD N DE VECES.
(define (mensaje texto contador n)
  (if (<= contador n) 
      (begin
        (display texto)
        (newline)
        (mensaje texto (+ contador 1) n)
        )
      (display "fin\n")
   ) 
)

(display "Ingrese la Cantidad N veces a repetir A: ")
(mensaje "A" 1 (read))

    

;EJERCICIO 2 NUMERO PERFECTO
(define (perfecto n cont acu)
 
  (if (and (> n cont)(= (remainder n cont) 0))
      (perfecto n(+ cont 1)(+ acu cont))
      (if (>= n cont)
          (perfecto n (+ cont 1) acu)
          (if (= acu n)#t #f
              )
          )))