;;===============( Map Generation )===============;;
(define (make-row width)
  (make-vector width #f))
(define (make-empty-map width height)
  (let ([amap (make-vector height)])
    (for-each
     (lambda [y]
       (vector-set! amap y (make-row width)))
     (range 0 height))
    amap))


(define (digCallback freeCells gameMap)
  (lambda [x y cell]
    (let ([key (num-pair->key x y)])
      (cond
       [(= cell 2) ;door
        (set-add! freeCells key)
        (game-map-set! gameMap x y "+")]
       [(= cell 0) ;floor
        (set-add! freeCells key)
        (game-map-set! gameMap x y ".")]
       [(= cell 3) ;corridor
        (set-add! freeCells key)
        (game-map-set! gameMap x y "\"")]
       [(= cell 1) (game-map-set! gameMap x y "#")]))))


(define (digCallback-third freeCells gameMap)
  (define *x-margin* (floor (div (- *map-width* (* *map-width* 0.6)) 2)))
  (define *y-margin* (floor (div (- *map-height* (* *map-height* 0.6)) 2)))
  (lambda [x y cell]
    (let* ([x (+ x *x-margin*)]
           [y (+ y *y-margin*)]
           [key (num-pair->key x y)])
      (cond [(= cell 1)
             (set-add! freeCells key)
             (game-map-set! gameMap x y "~")]
            [(and (not (game-map-ref gameMap x y #f))
                  (near-free-cell? freeCells x y))
             (game-map-set! gameMap x y "#")]))))

(define (map-gen seed)
  (\> ROT
      'RNG
      `(setSeed ,seed))
  (let ([aMap (js-new "ROT.Map.Rangersheck" *map-width* *map-height*)]
        [freeCells (make-set)]
        [gameMap  (make-empty-map *map-width* *map-height*)]
        [bMap (js-new "ROT.Map.Cellular" (* 0.6 *map-width* ) (* 0.6 *map-height*))])
    (.. create aMap (js-closure (digCallback freeCells gameMap)))
    (.. randomize bMap 0.45)
    (for-each (lambda (_)
           #|(.. create (car room)
           (js-closure (digCallback-second freeCells gameMap)))|#
           (.. create bMap (js-closure (digCallback-third freeCells gameMap)))
                ) (iota 6))
    (console-log gameMap)
    (values freeCells gameMap)))

(define (room-gen-random)
  (let ([door-x (random-int (- *map-width* 20)  1)]
        [door-y (random-int (- *map-height* 7) 1)])
    (cons
     (\> ROT
        'Map
        'Feature
        'Room
        `(createRandomAt ,door-x
                         ,door-y
                         ,(random-item (vector 1 -1))
                         ,(random-item (vector 1 -1))
                         ,(js-obj "roomWidth" (vector (random-int 4 5)
                                                      (random-int 20 5))
                                  "roomHeight" (vector (random-int 3 3)
                                                       (random-int 7 5)))))
     (cons door-x door-y))))
