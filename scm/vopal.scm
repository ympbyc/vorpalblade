;;;;        You found a vorpal blade!
;;;;
;;;;              ############
;;;;              #..........#
;;;;              #....@.....#
;;;;              #.......>..#
;;;;              ############
;;;;
;;;; ympbyc the Programmer    Chaotic human hacker

;;===================( Config )===================;;
(define *map-width* 64)
(define *map-height* 32)
(define *seed* 1236)
(define *path-cache-duration* 10)
(define *visibility-distance* 5)
(define *GAME-display* (js-new "ROT.Display" (js-obj  "fontSize" 16
                                                      "fontFamily" "Monaco"
                                                      "textBaseline" "middle"
                                                      "lineHeight" "2em"
                                                      "width" *map-width*
                                                      "height" *map-height*)))
(define *GAME-engine* (js-new "ROT.Engine"))
(define *keymap* (construct-eq-hashtable
                  89 7    ;y
                  75 0    ;k
                  85 1    ;u
                  76 2    ;l
                  78 3    ;n
                  74 4    ;j
                  66 5    ;b
                  72 6))  ;h
(define *objects* (construct-eq-hashtable
                   'player #f
                   'enemies '()))
(define (game-player) (hashtable-ref *objects* 'player #f))
(define (game-enemies) (hashtable-ref *objects* 'enemies #f))

(define *light-pass-char* (set "." "~"))
(define *creature-pass-char* (set "." "~" "+"))



;;===============( Map Generation )===============;;
;;Although some mutations are performed, CLEAN AND FUNCTIONAL as a whole.

(define (digCallback freeCells gameMap)
  (lambda [x y cell]
    (let ([key (num-pair->key x y)])
      (cond
       [(hashtable-ref gameMap key #f)
        (set-add! freeCells key)
        (hashtable-set! gameMap key "~")]
       [(= cell 0) ;floor
        (set-add! freeCells key)
        (hashtable-set! gameMap key ".")]
       [(= cell 2) ;door
        (set-add! freeCells key)
        (hashtable-set! gameMap key "+")]
       [(not (eqv? "#" (hashtable-ref gameMap key "#"))) ;;if cell conflicts, make it a floor
        (set-add! freeCells key)
        (hashtable-set! gameMap key (hashtable-ref gameMap key "#"))]
       [(= cell 1) (hashtable-set! gameMap key "#")]))))

(define (map-gen seed)
  (\> ROT
      'RNG
      `(setSeed ,seed))
  (let ([aMap (js-new "ROT.Map.Rangersheck" *map-width* *map-height*)]
        [rooms (map room-gen-random (iota 4))] ;;;;;
        [freeCells (make-set)]
        [gameMap  (make-eq-hashtable)])
    (.. create aMap (js-closure (digCallback freeCells gameMap)))
    (map (lambda (room) (.. create (car room) (js-closure (digCallback freeCells gameMap)))) rooms) ;;;;
    (values freeCells gameMap)))

(define (room-gen-random)
  (let ([door-x (random-int (- *map-width* 10)  1)]
        [door-y (random-int (- *map-height* 5) 1)])
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
                                                      (random-int 15 5))
                                  "roomHeight" (vector (random-int 3 3)
                                                       (random-int 7 5)))))
     (cons door-x door-y))))


;;====================( FOV )=====================;;
(define (light-passes? gameMap)
  (define-generic l-pass #t) ;memoize
  (define-method l-pass (x y)
    (let ([cell (hashtable-ref gameMap (num-pair->key x y) "#")])
      (set-contains? *light-pass-char* cell)))
  l-pass)

(define memoized-light-passes #f)

(define (draw-fov gameMap freeCells pl-x pl-y)
  (or memoized-light-passes (set! memoized-light-passes (light-passes? gameMap)))
  (let ([fov (js-new "ROT.FOV.PreciseShadowcasting" memoized-light-passes)])
    (.. compute fov pl-x pl-y *visibility-distance*
        (js-lambda (x y)
                   (draw-colored-char x y (hashtable-ref gameMap (num-pair->key x y) "#"))))))


;;=================( Datatype  )==================;;
;;player has to be represented as a js object due to the lib we are using
(define-class <rot-actor> '() (lambda [x]
                               (and (clos-slot-exists x 'getSpeed "function")
                                    (clos-slot-exists x 'act "function"))))
(define-class <creature> `(,<rot-actor>) (lambda [p]
                            (and (clos-slot-exists p 'x "number")
                                 (clos-slot-exists p 'y "number"))))
(define-class <player> `(,<creature>))
(define-class <enemy>  `(,<creature>)
  (lambda (e)
    (and (clos-slot-exists e '_cached_path)
         (clos-slot-exists e '_cache_usage_count "number"))))

(define (creature-x cr)
  (js-ref cr 'x))
(define (creature-y cr)
  (js-ref cr 'y))


;;==================(  Visual )===================;;
(define *char-color* (construct-eq-hashtable
                      "#" "#222"
                      "." "#fff"
                      "+" "#f57125"
                      "~" (make-rgb 135 115 255))) ;blue
(define *char-bg-color* (construct-eq-hashtable
                         "#" "#c0a9b3"
                         "." "#12122c"
                         "+" "#752612"
                         "~" (make-rgb 38 54 138))) ;light blue
(define (char-color ch)
  (hashtable-ref *char-color* ch "#fff"))
(define (char-bg-color ch)
  (hashtable-ref *char-bg-color* ch "#fff"))

(define (draw-colored-char x y ch)
  (if (eqv? ch "~")
      (draw-colored-char-variation x y ch) ;;;;;to-enhance
      (.. draw *GAME-display* x y ch (char-color ch) (char-bg-color ch))))
(define (draw-colored-char-variation x y ch)
  (.. draw *GAME-display* x y ch
      (rgb->css-string (random-close-color (char-color ch) 40))
      (rgb->css-string (random-close-color (char-bg-color ch) 40))))

;;=====================( IO )=====================;;
(define (draw-whole-map gameMap)
  (vector-for-each (lambda [key]
                     (let ([c  (map string->number (string-split key ","))]
                           [chr (hashtable-ref gameMap key "#")])
                       (draw-colored-char (car c) (cadr c) chr)))
                   (hashtable-keys gameMap)))

(define-generic creature-draw)
(define-method  creature-draw ([pl <player>])
  (.. draw *GAME-display*
      (creature-x pl)
      (creature-y pl)
      "@"
      "#ff0"))
(define-method  creature-draw ([en <enemy>])
  (.. draw *GAME-display*
      (creature-x en)
      (creature-y en)
      "j"
      "red"))


;;=================( Creatures )==================;;
(define (make-creature <class> x y gameMap freeCells)
  (letrec ([creature (make <class>
                       (js-obj
                        "x"        x
                        "y"        y
                        "getSpeed" (js-lambda [] (js-call get-speed creature))
                        "act"      (js-lambda [] (js-call rot-act creature gameMap freeCells))
                        "_cached_path" '#()
                        "_cache_usage_count" *path-cache-duration*))])
    creature))


;;=================( Actor Spec )=================;;
(define-generic get-speed)
(define-method get-speed ([pl <player>]) 100)
(define-method get-speed ([pl <enemy>]) 100)

(define-generic rot-act)


(define-method rot-act ([pl <player>] gameMap freeCells)
  (.. lock *GAME-engine*)
  (add-handler-once! "body"
                     "keydown"
                     (lambda [e]
                       (player-movement e pl gameMap freeCells)
                       (draw-fov gameMap freeCells (creature-x pl) (creature-y pl))
                       (js-call creature-draw pl)
                       (.. unlock *GAME-engine*))))

(define-method rot-act ([en <enemy>] gameMap freeCells)
  (enemy-movement en gameMap freeCells)
  (js-call creature-draw en))


;;=============( Creature Generation )============;;
;;clean and functional
(define (creature-init <class> gameMap freeCells)
  (let* ([index (floor (* (\> ROT 'RNG '(getUniform))
                          (set-size freeCells)))]
         [key (vector-ref (set-contents freeCells) index)]
         [parts (string-split key ",")]
         [x (string->number (car parts))]
         [y (string->number (cadr parts))])
    (make-creature <class> x y gameMap freeCells)))


;;==============( Player Movement )===============;;
;;Draws to the map
;;mutates player object
(define (player-movement e pl gameMap freeCells)
  (let ([direction (hashtable-ref *keymap* (js-ref e 'keyCode) #f)])
    (if direction
        (let* ([diff (vector-ref (js-ref (js-ref ROT 'DIRS) "8") direction)]
               [cur-x (creature-x pl)]
               [cur-y (creature-y pl)]
               [new-x (+ cur-x (vector-ref diff 0))]
               [new-y (+ cur-y (vector-ref diff 1))]
               [new-key (num-pair->key new-x new-y)])
          (if (set-contains? freeCells new-key)
              (let ([char (hashtable-ref gameMap (num-pair->key cur-x cur-y) ".")])
                (draw-colored-char cur-x cur-y char)
                (js-set! pl "x" new-x)
                (js-set! pl "y" new-y))
              pl))
        pl)))

;;===============( Enemy Movement )===============;;
(define (char-passable? ch)
  (set-contains? *creature-pass-char* ch))

(define (enemy-movement en gameMap freeCells)
  (define passbale-callback (js-lambda [x y]
     (char-passable? (hashtable-ref gameMap (num-pair->key x y) #f))))
  (define (path-callback path) (js-lambda [x y]
                                          (.. push path (vector x y))))
  (let* ([x (creature-x (game-player))]
         [y (creature-y (game-player))]
         [cur-x (creature-x en)]
         [cur-y (creature-y en)]
         [cur-key (num-pair->key cur-x cur-y)]
         [path (make-vector 0)]
         [astar (js-new "ROT.Path.AStar" x y passbale-callback (js-obj "topology" 4))]
         [c-usage-c (js-ref en "_cache_usage_count")]
         [c-path (js-ref en "_cached_path")])
    (set-add! freeCells cur-key)         ;free current cell
    (cond [(and (< c-usage-c *path-cache-duration*) (> (vector-length c-path) 5))
           (js-set! en "_cache_usage_count" (+ c-usage-c 1))
           (set! path c-path)]
          [else
           (js-set! en "_cache_usage_count" 0)
           (.. compute astar cur-x cur-y (path-callback path))])
    (.. shift path) ;remove current position
    (cond [(<= (vector-length path) 1)
           (.. lock *GAME-engine*)
           (display "GAME OVER")]
          [else
           (let* ([new-x (vector-ref (vector-ref path 0) 0)]
                  [new-y (vector-ref (vector-ref path 0) 1)]
                  [char (hashtable-ref gameMap cur-key ".")]
                  [new-key (num-pair->key new-x new-y)])
             (draw-colored-char cur-x cur-y char) ;free current cell
             (js-set! en "_cached_path" path)
             (when (set-contains? freeCells new-key)
                   (set-remove! freeCells (num-pair->key new-x new-y)) ;reserve
                   (js-set! en "x" new-x)
                   (js-set! en "y" new-y)))])))

;;================( Game Engine )=================;;
(define (game-init gameMap freeCells)
  (let ([pl (creature-init <player> gameMap freeCells)])
    (.. addActor *GAME-engine* pl)
    (hashtable-set! *objects* 'player pl)
    (js-call creature-draw pl))
  (hashtable-set! *objects* 'enemies
                  (map (lambda (_)
                         (let ([en (creature-init <enemy> gameMap freeCells)])
                           (.. addActor *GAME-engine* en)
                           (js-call creature-draw en)
                           en)) (iota 2)))
  (.. start *GAME-engine*))


;;===================(  Main )====================;;
((lambda []
   (element-insert! "#rot-container" (.. getContainer *GAME-display*)) ;;add canvas to html
   (let-values ([[freeCells gameMap] (map-gen *seed*)])
     ;(draw-whole-map gameMap)
     (game-init gameMap freeCells))))
