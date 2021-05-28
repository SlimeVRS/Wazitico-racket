#lang racket/gui
(require "graphLogic.rkt")

(define (shrt)
  (send msg set-label (number->string (car (send list_start get-selections))))
  (send msg1 set-label (number->string (car (send list_end get-selections))))
  (pre_shorter (car (send list_start get-selections)) (car (send list_end get-selections)))
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

(define msg (new message%
                 [parent col1]
                 [label "Start"]
                 )
  )
(define msg1 (new message%
                 [parent col1]
                 [label "End"]
                 )
  )

(define list_start(new list-box%
                       [parent col2]
                       [label "Inicio"]
                       [choices '("a" "b" "c" "d")]
                       )
  )

(define list_end(new list-box%
                     [parent col2]
                     [label "Fin"]
                     [choices '("a" "b" "c" "d")]
                     )
  )

(new button% [parent col2]
             [label "Calcular"]
             [callback (lambda (button event)
                         (shrt)
                         )])

(new button% [parent col2]
             [label "Salir"]
             [callback (lambda (button event)
                         (send frame show #f))])

(send frame show #t)
