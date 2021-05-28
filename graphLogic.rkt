#lang racket

(provide (all-defined-out))

#|
Add a node with no neighbours to the graph
I: Id or name of the node and the graph
O: The graph with the new node added 
R: The name of the graph should be a string and the graph has to be correctly written
|#
(define (addNode id graph)
  (cond((null? graph)
        (list (append (list id) (list graph))))
        (else
         (append graph (list (list id '()))))     
 ))

#|
Add a connection between two nodes with the given weight
I: Start node, end node, weight and the graph
O: The graph with a new route 
R: The nodes given have to be on the graph and the weight should be a number 
|#

(define (addRoutes start end weight graph)
  (addRoutes-Aux start end weight graph '())
  )

#|
Auxiliary function of addRoutes
|#

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

#|
Creates a new route as a list according to the node given
I: The name of the node and the list of the routes
O: Node list with its connections 
R: The node has to be defined on the graph
|#
(define (changeRoutes id _list)
  (changeRoutes-Aux id _list)
  )

(define (changeRoutes-Aux id _list)
  (cond((null? _list)
        (list(append _list id)))
       (else
        (append _list (list id)))))
#|
Validates if a path reaches the goal node or not
I: The goal node and the path
O: True or false
R: The route has to be a list
|#
(define (solution? end route)
  (equal? end (car route))
  )

#|
Validates if a path with routes reaches the goal node or not
I: The goal node and the path
O: True or false
R: The route has to be a list
|#
(define (weightedSolution? end route)
  (equal? end (caar route))
  )

#|
Returns the neighbours of the node if it exist
I: A node and the graph
O: The neigbours of the given node
R: The node should exists, otherwise, will return false 
|#
(define (neighbours node graph)
  (neighbours-Aux (assoc node graph) node graph)
  )

#|
Auxiliary function of neighbours
|#
(define (neighbours-Aux result node graph)
  (cond((equal? result #f)
        #f)
       (else
        (cond((null? (cdr result))
              (cdr result))
             (else
              (cadr result))))))

#|
Validate if a element exists on a list
I: A node and the list
O: True if the element exist, false otherwise
|#
(define (member? node _list)
  (cond((null? _list)
        #f)
       ((equal? node (car _list))
        #t)
       (else
        (member? node (cdr _list)))))

#|
Validate if a node with route exists on a list
I: A node with weight and the list
O: True if the element exist, false otherwise
R: The node should have routes added
|#
(define (memberWeight? node list)
  (cond((null? list)
        #f)
       ((equal? (car node) (caar list))
        #t)
       (else
        (memberWeight? node (cdr list)))))

#|
Gets all the neighbours of each node of the route
I: Route and the graph
O: A list of all the n
R: The route should be a list
|#
(define (checkNeighbours route graph)
  (checkNeighbours-Aux route '() graph (neighbours(car route) graph))
  )

#|
Auxiliary function of checkNeighbours
|#
(define (checkNeighbours-Aux route result graph neighbours)
  (cond((null? neighbours)
        result)
       (else
        (cond((member? (caar neighbours) route)
              (checkNeighbours-Aux route result graph (cdr neighbours)))
             (else
              (checkNeighbours-Aux route (append (list (cons (caar neighbours) route)) result) graph (cdr neighbours)))))))

#|
Gets all the neighbours of each node with their weight of the route
I: Route and the graph
O: A list of all the n
R: The route needs to have the weight of each path
|#
(define (extend route graph)
  (extend-Aux route '() graph (neighbours (caar route) graph))
  )

#|
Auxiliary function of extend
|#
(define (extend-Aux route result graph neighbours)
  (cond((null? neighbours) result)
       (else
        (cond((memberWeight? (car neighbours) route)
              (extend-Aux route result graph (cdr neighbours)))
             (else
              (extend-Aux route (append (list (cons (car neighbours)  route)) result) graph (cdr neighbours))
              )))))

#|
Invert the route given
I: A result and a route
O: The reversed route
R: Result has to be empty
|#
(define (invert result routes)
  (cond((null? routes)
        result)
       (else
        (invert
         (append (list (reverse (car routes))) result)
         (cdr routes)))))

#|
Returns all the routes from a to b node
I: Start and end node and the graph
O: All the routes from start to the end node
R: Graph well defined
|#
(define (searchAllRoutes start end graph)
  (allRoutes(list (list start)) end graph '())
  )

#|
BFS implementation
I: start and end node and the graph
O: The route from start to end
R: 
|#
(define (profNoWeight start end graph)
  (profNoWeight-Aux (list (list start)) end graph '())
  )

#|
Auxiliary function of profNoWeight
|#
(define (profNoWeight-Aux routes end graph result)
  (cond((null? routes)
        (invert '() result))
       ((solution? end (car routes))
        (profNoWeight-Aux (cdr routes)
                          end
                          graph
                          (cons (car routes) result)))
       (else
        (profNoWeight-Aux (append (checkNeighbours (car routes) graph) (cdr routes))
                          end
                          graph
                          result))))

#|
BFS implementation but checking the weight
I: start and end node and graph
O: The route with each weight of the node to the other
R: 
|#
(define (profWeight start end graph)
  (profWeight-Aux (list (list (list start '0))) end graph '())
  )

#|
Auxiliary function of profWeight
|#
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

#|
Gives the distance of the node
I: One or more route
O: A list with the distance
R: 
|#
(define (getDistance routes)
  (getDistance-Aux routes '())
  )

#|
Auxiliary function of distance
|#
(define (getDistance-Aux routes resut)
  (cond((null? routes)
        resut)
       (else
        (getDistance-Aux (cdr routes) (append resut (list(totalDistance 0 (car routes))))))))

#|
Gives the total distance of a route
I: A result and the route
O: The sum of the distances of the route
R: Result should start at 0
|#
(define (totalDistance result route)
  (cond((null? route)
        result)
       (else
        (totalDistance (+ result (cadar route)) (cdr route)))))

#|
Gives the minimun value of a route
I: route
O: its minimum
R: list of numbers
|#
(define (minimum list)
  (minimum-Aux list (car list) 0)
  )

#|
Auxiliary function of minimum
|#
(define (minimum-Aux list weight index)
  (cond((null? list)
        index)
       (else
        (cond((<= weight (car list))
              (minimum-Aux (cdr list) weight index))
             (else
              (minimum-Aux (cdr list) (car list) (+ index 1)))))))

#|
Return the shortest route from node a to node b of a given graph
I: A start and end node and a graph
O: The shortest route
R: 
|#
(define (shorterRoute start end graph)
  (shorterRoute-Aux (profWeight start end graph) (profNoWeight start end graph))
  )

#|
Auxiliary function of shorterRoute
|#
(define (shorterRoute-Aux routesW routes)
  (cond((null? routesW)
        routesW)
       (else
        (shorterRoute-Aux2 (minimum (getDistance routesW)) routesW routes))))

#|
Auxiliary function of shorterRoute-Aux
|#
(define (shorterRoute-Aux2 weight routeW routes)
  (cond((zero? weight)
        (cons (car routes) (list (totalDistance 0 (car routeW)))))
       (else
        (shorterRoute-Aux2 (- weight 1) (cdr routeW) (cdr routes)))))

#|
Returns all the routes from start to end
I: Start and end node and the graph
O: Routes from start to end
R: Graph well defined
|#
(define (allRoutes start end graph)
  (sorter (allRoutes-Aux (profNoWeight start end graph) (getDistance (profWeight start end graph)) '()) '())
  )

#|
Auxiliary function for allRoutes
|#
(define (allRoutes-Aux routes routesW result)
  (cond((null? routes)
        result)
       (else
        (allRoutes-Aux (cdr routes) (cdr routesW)  (append result (list (cons (car routes) (list (car routesW)))))))))

#|
Insert a node acorting to its value
I: A node and a list
O: Ordered list
R: The list with the node added in the respective position
|#
(define (insert node _list)
  (cond((null? _list)
        (list node))
       ((> (cadr node) (cadar _list))
        (cons (car _list)
              (insert node (cdr _list))))
       (else
        (cons node _list))))

#|
Gives a list with the routes oredered from smaller to greater
I: List with routes and a list
O: Ordered list
R: 
|#
(define (sorter routes sortedList)
  (cond((null? routes)
        sortedList)
        (else
         (sorter (cdr routes) (insert (car routes) sortedList)))))