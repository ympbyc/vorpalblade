(define (room-gen-random)
  (let ([door-x (random-int (- *map-width* 1)  1)]
        [door-y (random-int (- *map-height* 1) 1)])
    (cons
     (\> ROT
        'Map
        'Feature
        'Room
        `(createRandomAt ,door-x
                         ,door-y
                         ,(random-item (vector 1 -1))
                         ,(random-item (vector 1 -1))
                         ,(js-obj "roomWidth" (vector (random-int 20 5)
                                                      (random-int 30 5))
                                  "roomHeight" (vector (random-int 15 5)
                                                       (random-int 15 5)))))
     (cons door-x door-y))))

(define (connect-doors door1 door2 cb)
  (let ([corridor (js-new "ROT.Map.Feature.Corridor" (car door1) (cdr door1) (car door2) (cdr door2))])
    (.. create corridor cb)))

(define (map-gen seed)
  (\> ROT 'RNG `(setSeed ,seed))
  (let* ([rooms (map room-gen-random (iota 7))]
         [freeCells (make-vector 0)]
         [gameMap (make-eq-hashtable)]
         [digCb (js-closure (digCallback freeCells gameMap))]
         [doors (map (lambda (room) (.. create (car room) digCb)
                                (cdr room)) rooms)])
    (fold-right (lambda (door1 door2)
                  (connect-doors door1 door2 digCb)
                  door1) (car doors) (cdr doors))
    (values freeCells gameMap)))
