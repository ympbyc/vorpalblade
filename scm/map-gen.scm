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

(define gm (make-empty-map 5 5))
(game-map-set! gm 2 2 "@")
(console-log gm)

(define (digCallback-second freeCells gameMap)
  (lambda [x y cell]
    (let ([key (num-pair->key x y)])
      (cond
       [(game-map-ref gameMap x y #f)
        (set-add! freeCells key)
        (game-map-set! gameMap x y "~")]
       [(= cell 2) ;door
        (set-add! freeCells key)
        (game-map-set! gameMap x y "+")]
       [(= cell 0) ;floor
        (set-add! freeCells key)
        (game-map-set! gameMap x y ".")]
       [(= cell 1)
        (set-remove! freeCells key)
        (game-map-set! gameMap x y "#")]))))

(define (map-gen seed)
  (\> ROT
      'RNG
      `(setSeed ,seed))
  (let ([aMap (js-new "ROT.Map.Rangersheck" *map-width* *map-height*)]
        [rooms (map room-gen-random (iota 5))] ;;;;;
        [freeCells (make-set)]
        [gameMap  (make-empty-map *map-width* *map-height*)])
    (.. create aMap (js-closure (digCallback freeCells gameMap)))
    (map (lambda (room)
           (.. create (car room)
               (js-closure (digCallback-second freeCells gameMap)))) rooms) ;;;;
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
