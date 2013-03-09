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
(define *map-width* 60)
(define *map-height* 24)
(define *seed* 128)
(define *GAME-display* (js-new "ROT.Display" (js-obj  "fontSize" 13
                                                      "fontFamily" "Osaka"
                                                      "fontStyle" "bold"
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


;;===============( Map Generation )===============;;
;;Although some mutations are performed, CLEAN AND FUNCTIONAL as a whole.

(define (digCallback freeCells gameMap)
  (lambda [x y cell]
    (let ([key (num-pair->key x y)])
      (cond
       [(= cell 0) ;floor
        (vector-set! freeCells (vector-length freeCells) key)
        (hashtable-set! gameMap key ".")]
       [(= cell 2) ;door
        (vector-set! freeCells (vector-length freeCells) key)
        (hashtable-set! gameMap key "+")]
       [(not (eqv? "#" (hashtable-ref gameMap key "#"))) ;;if cell conflicts, make it a floor
        (hashtable-set! gameMap key ".")]
       [else (hashtable-set! gameMap key "#")]))))

(define (map-gen seed)
  (\> ROT
      'RNG
      `(setSeed ,seed))
  (let ([aMap (js-new "ROT.Map.Digger" *map-width* *map-height*)]
        [freeCells (make-vector 0)]
        [gameMap  (make-eq-hashtable)])
    (.. create aMap (js-closure (digCallback freeCells gameMap)))
    (values freeCells gameMap)))

;;=================( Datatype  )==================;;
;;player has to be represented as a js object due to the lib we are using
(define-class <rot-actor> '() (lambda [x]
                               (and (clos-slot-exists x 'getSpeed "function")
                                    (clos-slot-exists x 'act "function"))))
(define-class <creature> `(,<rot-actor>) (lambda [p]
                            (and (clos-slot-exists p 'x "number")
                                 (clos-slot-exists p 'y "number"))))
(define-class <player> `(,<creature>))
(define-class <enemy>  `(,<creature>))

(define (creature-x cr)
  (js-ref cr 'x))
(define (creature-y cr)
  (js-ref cr 'y))


;;==================(  Visual )===================;;
(define *char-color* (construct-eq-hashtable
                      "#" "#222"
                      "." "#fff"
                      "+" "#f57125"))
(define *char-bg-color* (construct-eq-hashtable
                         "#" "#c0a9b3"
                         "." "#12122c"
                         "+" "#752612"))
(define (char-color ch)
  (hashtable-ref *char-color* ch "#fff"))
(define (char-bg-color ch)
  (hashtable-ref *char-bg-color* ch "#fff"))


;;=====================( IO )=====================;;
(define (draw-whole-map gameMap)
  (vector-for-each (lambda [key]
                     (let ([c  (map string->number (string-split key ","))]
                           [chr (hashtable-ref gameMap key "#")])
                       (.. draw *GAME-display* (car c) (cadr c) chr (char-color chr) (char-bg-color chr))))
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
      "P"
      "red"))


;;=================( Creatures )==================;;
(define (make-creature <class> x y gameMap freeCells)
  (letrec ([creature (make <class>
                       (js-obj
                        "x"        x
                        "y"        y
                        "getSpeed" (js-lambda [] (js-call get-speed creature))
                        "act"      (js-lambda [] (js-call rot-act creature gameMap freeCells))))])
    creature))


;;=================( Actor Spec )=================;;
(define-generic get-speed)
(define-method get-speed ([pl <player>]) 100)
(define-method get-speed ([pl <enemy>]) 50)

(define-generic rot-act)


(define-method rot-act ([pl <player>] gameMap freeCells)
  (.. lock *GAME-engine*)
  (add-handler-once! "body"
                     "keydown"
                     (lambda [e]
                       (player-movement e pl gameMap freeCells)
                       (js-call creature-draw pl)
                       (.. unlock *GAME-engine*))))

(define-method rot-act ([en <enemy>] gameMap freeCells)
  (define passbale-callback (js-lambda [x y]
     (eqv? "." (hashtable-ref gameMap (num-pair->key x y) #f))))
  (define (path-callback path) (js-lambda [x y]
                                          (.. push path (vector x y))))
  (let* ([x (creature-x (game-player))]
         [y (creature-y (game-player))]
         [cur-x (creature-x en)]
         [cur-y (creature-y en)]
         [path (make-vector 0)]
         [astar (js-new "ROT.Path.AStar" x y passbale-callback (js-obj "topology" 4))])
    (.. compute astar cur-x cur-y (path-callback path))
    (.. shift path) ;remove current position
    (cond [(<= (\> path 'length) 1)
           (.. lock *GAME-engine*)
           (display "GAME OVER")]
          [else
           (let ([new-x (vector-ref (vector-ref path 0) 0)]
                 [new-y (vector-ref (vector-ref path 0) 1)]
                 [char (hashtable-ref gameMap (num-pair->key cur-x cur-y) ".")])
             (.. draw *GAME-display* cur-x cur-y char (char-color char) (char-bg-color char))
             (js-set! en "x" new-x)
             (js-set! en "y" new-y)
             (js-call creature-draw en))])))


;;=============( Creature Generation )============;;
;;clean and functional
(define (creature-init <class> gameMap freeCells)
  (let* ([index (floor (* (\> ROT 'RNG '(getUniform))
                          (vector-length freeCells)))]
         [key (vector-ref freeCells index)]
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
          (if  (and (hashtable-ref gameMap new-key #f)
                    (> (.. indexOf freeCells new-key) -1))
               (let ([char (hashtable-ref gameMap (num-pair->key cur-x cur-y) ".")])
                 (.. draw *GAME-display* cur-x cur-y
                     char (char-color char) (char-bg-color char))
                 (js-set! pl "x" new-x)
                 (js-set! pl "y" new-y))
               pl))
        pl)))


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
     (draw-whole-map gameMap)
     (game-init gameMap freeCells))))
