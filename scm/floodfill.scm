(define (floodfill gameMap x y callback)
  (floodfill- gameMap x y (game-map-ref gameMap x y #f) callback))

(define (floodfill- gameMap x y val callback)
  (define (inner acc x y)
    (cond [(and (eqv? val (game-map-ref gameMap x y #f))
                (not (memv (num-pair->key x y)  acc)))
           (callback  x y)
           (letrec ([a (inner (cons (num-pair->key x y) acc) (- x 1) y)]
                    [b (inner a (+ x 1) y)]
                    [c (inner b x (- y 1))]
                    [d (inner c x (+ y 1))])
             d)]
          [else acc]))
  (inner x y))
