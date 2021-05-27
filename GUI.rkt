#lang racket/gui

(define(factorial value)
  (cond
    ((equal? value 0) 1)
    ((> value 0)(* value (factorial (- value 1))))
    )
  )

(define frame (new frame%
                   [label "Wazitco"]
                   [style '(hide-menu-bar)]
                   ))

(define row1
  (new horizontal-panel%
       [parent frame]
       [stretchable-height #t]
       ))

(define col1
  (new vertical-panel%
       [parent row1]
       [style       '(border)]
       [stretchable-height #t]
       ))

(define col2
  (new vertical-panel%
       [parent row1]
       [min-width 300]
       [style '(border hide-hscroll)]
       [stretchable-width #f]
       ))

(new list-box% [parent col2]
             [label "Inicio"]
             [choices '("a" "b")]
             )

(new list-box% [parent col2]
             [label "Fin"]
             [choices '("a" "b")]
             )

(new button% [parent col2]
             [label "Calcular"]
             [callback (lambda (button event)
                         (factorial))])

(new button% [parent col2]
             [label "Salir"]
             [callback (lambda (button event)
                         (send frame show #f))])

(send frame show #t)
