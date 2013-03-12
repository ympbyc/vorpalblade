(define (floodfill gameMap x y callback)
  (floodfill- gameMap x y (game-map-ref gameMap x y #f) callback))

(define (floodfill- gameMap x y val callback)
  (let ([acc '#()])
    ))
