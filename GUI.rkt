#lang racket/gui
(require "graphLogic.rkt")

(define (shrt)
  (send msg set-label (number->string (car (send list_start get-selections))))
  (send msg1 set-label (number->string (car (send list_end get-selections))))
  (pre_shorter (car (send list_start get-selections)) (car (send list_end get-selections)))
  )

(define (show rute)
  (cond
    ((null? rute) (send msg2 set-label "No hay ruta"))
    (else (show-aux rute))
    )
  )

(define (show-aux rute)
  (send msg2 set-label (number->string (cadr rute)))
  (paint-ruta (car rute))
  (paint-node (car rute))
  )

(define (paint-node ruta)
  (cond
    ((null? ruta) #t)
    (else (paint-node-aux (car ruta))(paint-node (cdr ruta)))
    )
  )

(define (paint-node-aux node)
  (send dc set-pen red-pen)
  (cond
    ((equal? node "a") (send dc draw-ellipse 5 5 55 55))
    ((equal? node "b") (send dc draw-ellipse 100 5 55 55))
    ((equal? node "c") (send dc draw-ellipse 5 100 55 55))
    ((equal? node "d") (send dc draw-ellipse 100 100 55 55))
    )
  )

(define (paint-ruta ruta)
  (cond
    ((null? (cdr ruta)) #t)
    (else (paint-ruta-aux ruta)(paint-ruta (cdr ruta)))
    )
  )

(define (paint-ruta-aux rute)
  (send dc set-pen red-pen)
  (cond
    ((equal? (car rute) "a") (cond
                               ((equal? (cadr rute) "b") (send dc draw-line 60 30 100 30)
                                                         (send dc draw-line 90 20 100 30)
                                                         (send dc draw-line 90 40 100 30)
                                                         )
                               ((equal? (cadr rute) "c") (send dc draw-line 30 60 30 100)
                                                         (send dc draw-line 20 90 30 100)
                                                         (send dc draw-line 40 90 30 100)
                                                         )
                               ((equal? (cadr rute) "d") (send dc draw-line 52 52 107 107)
                                                         (send dc draw-line 107 97 107 107)
                                                         (send dc draw-line 97 107 107 107)
                                                         )
                               ))
    ((equal? (car rute) "b") (cond
                               ((equal? (cadr rute) "c") (send dc draw-line 107 52 52 107)
                                                         (send dc draw-line 52 97 52 107)
                                                         (send dc draw-line 62 107 52 107)
                                                         )
                               ))
    ((equal? (car rute) "c") (cond
                               ((equal? (cadr rute) "d") (send dc draw-line 60 123 97 123)
                                                         (send dc draw-line 87 113 97 123)
                                                         (send dc draw-line 87 133 97 123)
                                                         )
                               ))
    ((equal? (car rute) "d") (cond
                               ((equal? (cadr rute) "b") (send dc draw-line 133 99 133 60)
                                                         (send dc draw-line 143 70 133 60)
                                                         (send dc draw-line 123 70 133 60)
                                                         )
                               ((equal? (cadr rute) "c") (send dc draw-line 107 145 78 163)
                                                         (send dc draw-line 78 163 52 145)
                                                         (send dc draw-line 62 145 52 145)
                                                         (send dc draw-line 52 155 52 145)
                                                         )
                               ))
    )
  )

(define no-pen (make-object pen% "BLACK" 1 'transparent))
(define no-brush (make-object brush% "BLACK" 'transparent))
(define black-pen (make-object pen% "BLACK" 3 'solid))
(define red-pen (make-object pen% "RED" 3 'solid))

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

(new message% [parent col1][label "Start: "])

(define msg (new message%
                 [parent col1]
                 [label ""]
                 )
  )

(new message% [parent col1][label "End: "])

(define msg1 (new message%
                 [parent col1]
                 [label ""]
                 )
  )

(new message% [parent col1][label "Distance: "])

(define msg2 (new message%
                 [parent col1]
                 [label ""]
                 )
  )

(define list_start(new list-box%
                       [parent col2]
                       [label "Inicio"]
                       [choices '("A (0)" "B (1)" "C (2)" "D (3)")]
                       )
  )

(define list_end(new list-box%
                     [parent col2]
                     [label "Fin"]
                     [choices '("A (0)" "B (1)" "C (2)" "D (3)")]
                     )
  )

(new button% [parent col2]
             [label "Calcular"]
             [callback (lambda (button event)
                         (send dc erase)
                         (repeat dc)
                         (show (shrt))
                         )])

(new button% [parent col2]
             [label "Salir"]
             [callback (lambda (button event)
                         (send frame show #f)
                         )])

(define canvas (new canvas% [parent col1]))

(define dc (send canvas get-dc))

(define (repeat dc)
  (send dc set-pen black-pen)
  (send dc set-brush no-brush)
  ;Text
  (send dc draw-text "A" 27 25)
  (send dc draw-text "B" 123 25)
  (send dc draw-text "C" 27 120)
  (send dc draw-text "D" 123 120)
  (send dc draw-text "7" 78 10)
  (send dc draw-text "5" 15 68)
  (send dc draw-text "3" 45 55)
  (send dc draw-text "2" 105 55)
  (send dc draw-text "1" 140 73)
  (send dc draw-text "4" 73 105)
  (send dc draw-text "6" 73 140)
  ;A
  (send dc draw-ellipse 5 5 55 55)
  ;C
  (send dc draw-ellipse 5 100 55 55)
  ;B
  (send dc draw-ellipse 100 5 55 55)
  ;D
  (send dc draw-ellipse 100 100 55 55)
  ;AB
  (send dc draw-line 60 30 100 30)
  (send dc draw-line 90 20 100 30)
  (send dc draw-line 90 40 100 30)
  ;AC
  (send dc draw-line 30 60 30 100)
  (send dc draw-line 20 90 30 100)
  (send dc draw-line 40 90 30 100)
  ;AD
  (send dc draw-line 52 52 107 107)
  (send dc draw-line 107 97 107 107)
  (send dc draw-line 97 107 107 107)
  ;BC
  (send dc draw-line 107 52 52 107)
  (send dc draw-line 52 97 52 107)
  (send dc draw-line 62 107 52 107)
  ;CD
  (send dc draw-line 60 123 97 123)
  (send dc draw-line 87 113 97 123)
  (send dc draw-line 87 133 97 123)
  ;DC
  (send dc draw-line 107 145 78 163)
  (send dc draw-line 78 163 52 145)
  (send dc draw-line 62 145 52 145)
  (send dc draw-line 52 155 52 145)
  ;DB
  (send dc draw-line 133 99 133 60)
  (send dc draw-line 143 70 133 60)
  (send dc draw-line 123 70 133 60)
  )

(send frame show #t)

(sleep/yield 1)

(repeat dc)