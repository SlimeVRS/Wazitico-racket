#lang racket

(provide (all-defined-out))

(define globalGraph '()
  )

(define (addNode id graph)
  (cond((null? graph)
        (list (append (list id) (list graph))))
        (else
         (append graph (list (list id '()))))     
 ))

(define (addRoutes start end weight graph)
  (addRoutes-Aux start end weight graph '())
  )

(define (addRoutes-Aux start end weight graph result)
  (cond((null? graph)
        result)
       (else
         (cond((equal? start (caar graph))
               (addRoutes-Aux start end weight (cdr graph)
                             (append result
                                     (list (append (list (caar graph))
                                                   (list (changeRoutes (list end weight) (cadar graph))))))))
              (else
               (addRoutes-Aux start end weight (cdr graph)
                              (append result
                                      (list(car graph)))))))))

(define (changeRoutes id _list)
  (changeRoutes-Aux id _list)
  )

(define (changeRoutes-Aux id _list)
  (cond((null? _list)
        (list(append _list id)))
       (else
        (append _list (list id)))))

(define (solution? end route)
  (equal? end (car route))
  )

(define (weightedSolution? end route)
  (equal? end (caar route))
  )

(define (neighbours node graph)
  (neighbours-Aux (assoc node graph) node graph)
  )

(define (neighbours-Aux result node graph)
  (cond((equal? result #f)
        #f)
       (else
        (cond((null? (cdr result))
              (cdr result))
             (else
              (cadr result))))))

(define (member? node _list)
  (cond((null? _list)
        #f)
       ((equal? node (car _list))
        #t)
       (else
        (member? node (cdr _list)))))

(define (memberWeight? node list)
  (cond((null? list)
        #f)
       ((equal? (car node) (caar list))
        #t)
       (else
        (memberWeight? node (cdr list)))))

(define (connect route graph)
  (connect-Aux route '() graph (neighbours (car route) graph))
  )

(define (connect-Aux route result graph neighbours)
  (cond((null? neighbours) result)
       (else
        (cond((member? (car neighbours) route)
              (connect-Aux route result graph (cdr neighbours)))
             (else
              (connect-Aux route (append (list (cons (car neighbours) route)) result) graph (cdr neighbours)))))))

(define (connectWeight route graph)
  (connectWeight-Aux route '() graph (neighbours(car route) graph))
  )

(define (connectWeight-Aux route result graph neighbours)
  (cond((null? neighbours) result)
       (else
        (cond((member? (caar neighbours) route)
              (connectWeight-Aux route result graph (cdr neighbours)))
             (else
              (connectWeight-Aux route (append (list (cons (caar neighbours) route)) result) graph (cdr neighbours)))))))

(define (extend route graph)
  (extend-Aux route '() graph (neighbours (caar route) graph))
  )

(define (extend-Aux route result graph neighbours)
  (cond((null? neighbours) result)
       (else
        (cond((memberWeight? (car neighbours) route)
              (extend-Aux route result graph (cdr neighbours)))
             (else
              (extend-Aux route (append (list (cons (car neighbours)  route)) result) graph (cdr neighbours))
              )))))
        
(define (invert routes _routes)
  (cond((null? _routes)
        routes)
       (else
        (invert
         (append (list (reverse (car _routes))) routes)
         (cdr _routes)))))

(define (prof start end graph)
  (allRoutes(list (list start)) end graph '())
  )

(define (prof-aux routes end graph result)
  (cond((null? routes)
        (invert '() result))
       ((solution? end (car routes))
        (prof-aux (cdr routes)
                  end
                  graph
                  (cons (car routes) result)))
       (else
        (prof-aux (append(connect(car routes) graph)
                         (cdr routes))
                  end
                  graph
                  result))))

(define (profNoWeight start end graph)
  (profNoWeight-Aux (list (list start)) end graph '())
  )

(define (profNoWeight-Aux routes end graph result)
  (cond((null? routes)
        (invert '() result))
       ((solution? end (car routes))
        (profNoWeight-Aux (cdr routes)
                          end
                          graph
                          (cons (car routes) result)))
       (else
        (profNoWeight-Aux (append (connectWeight (car routes) graph) (cdr routes))
                          end
                          graph
                          result))))

(define (profWeight start end graph)
  (profWeight-Aux (list (list (list start '0))) end graph '())
  )

(define (profWeight-Aux routes end graph result)
  (cond((null? routes)
        (invert '() result))
       ((weightedSolution? end (car routes))
        (profWeight-Aux (cdr routes)
                        end
                        graph
                        (cons (car routes) result)))
       (else
        (profWeight-Aux (append (extend (car routes) graph) (cdr routes))
                        end
                        graph
                        result))))

(define (distance routes)
  (distance-Aux routes '())
  )
 
(define (distance-Aux routes resut)
  (cond((null? routes)
        resut)
       (else
        (distance-Aux (cdr routes) (append resut (list(totalDistance 0 (car routes))))))))

(define (totalDistance weight route)
  (cond((null? route)
        weight)
       (else
        (totalDistance (+ weight (cadar route)) (cdr route)))))

(define (minimum list)
  (minimum-Aux list (car list) 0)
  )

(define (minimum-Aux list weight index)
  (cond((null? list)
        index)
       (else
        (cond((<= weight (car list))
              (minimum-Aux (cdr list) weight index))
             (else
              (minimum-Aux (cdr list) (car list) (+ index 1)))))))

(define (shorterRoute start end graph)
  (shorterRoute-Aux (profWeight start end graph) (profNoWeight start end graph))
  )

(define (shorterRoute-Aux routesW routes)
  (cond((null? routesW)
        routesW)
       (else
        (shorterRoute-Aux2 (minimum (distance routesW)) routesW routes))))

(define (shorterRoute-Aux2 weight routeW routes)
  (cond((zero? weight)
        (cons (car routes) (list (totalDistance 0 (car routeW)))))
       (else
        (shorterRoute-Aux2 (- weight 1) (cdr routeW) (cdr routes)))))

(define (allRoutes start end graph)
  (sorter (allRoutes-Aux (profNoWeight start end graph) (distance (profWeight start end graph)) '()) '())
  )

(define (allRoutes-Aux routes routesW result)
  (cond((null? routes)
        result)
       (else
        (allRoutes-Aux (cdr routes) (cdr routesW)  (append result (list (cons (car routes) (list (car routesW)))))))))

(define (insert node _list)
  (cond((null? _list)
        (list node))
       ((> (cadr node) (cadar _list))
        (cons (car _list)
              (insert node (cdr _list))))
       (else
        (cons node _list))))

(define (sorter routes sortedList)
  (cond((null? routes)
        sortedList)
        (else
         (sorter (cdr routes) (insert (car routes) sortedList)))))