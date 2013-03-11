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

(define (within-distance? cr1 cr2 distance)
  (and
   (< (abs (- (creature-x cr1) (creature-x cr2))) distance)
   (< (abs (- (creature-y cr1) (creature-y cr2))) distance)))


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
  (when (within-distance? (game-player) en *visibility-distance*)
      (js-call creature-draw en)))


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
          (set-add! freeCells cur-key) ;free current cell
          (if (set-contains? freeCells (num-pair->key cur-x cur-y))
              (let ([char (game-map-ref gameMap cur-x cur-y ".")])
                (draw-colored-char cur-x cur-y char)
                (set-remove! freeCells new-key) ;reserve new cell
                (js-set! pl "x" new-x)
                (js-set! pl "y" new-y))
              pl))
        pl)))

;;===============( Enemy Movement )===============;;
(define (char-passable? ch)
  (set-contains? *creature-pass-char* ch))

(define (enemy-movement en gameMap freeCells)
  (define passbale-callback (js-lambda [x y]
        (char-passable? (game-map-ref gameMap x y #f))))
  (define (path-callback path) (js-lambda [x y]
                                          (.. push path (vector x y))))
  (let* ([x (creature-x (game-player))]
         [y (creature-y (game-player))]
         [cur-x (creature-x en)]
         [cur-y (creature-y en)]
         [cur-key (num-pair->key cur-x cur-y)]
         [path (make-vector 0)]
         [astar (js-new "ROT.Path.AStar" x y passbale-callback (js-obj "topology" 8))]
         [c-usage-c (js-ref en "_cache_usage_count")]
         [c-path (js-ref en "_cached_path")])
    (set-add! freeCells cur-key)         ;free current cell
    (cond [(and (< c-usage-c *path-cache-duration*) (> (vector-length c-path) 5))
           (js-set! en "_cache_usage_count" (+ c-usage-c 1))
           (set! path c-path)]
          [else
           (js-set! en "_cache_usage_count" 0)
           (.. compute astar cur-x cur-y (path-callback path))])
    (cond [(<= (vector-length path) 0)
           ;(.. lock *GAME-engine*)
           ;(display "Bites!")
           (.. removeActor *GAME-engine* en)]
          [(> (vector-length path) 2)
           (.. shift path) ;remove current position
           (let* ([new-x (vector-ref (vector-ref path 0) 0)]
                  [new-y (vector-ref (vector-ref path 0) 1)]
                  [char (game-map-ref gameMap cur-x cur-y ".")]
                  [new-key (num-pair->key new-x new-y)])
             (draw-colored-char cur-x cur-y char) ;free current cell
             (js-set! en "_cached_path" path)
             (when (set-contains? freeCells new-key)
                   (set-remove! freeCells (num-pair->key new-x new-y)) ;reserve
                   (js-set! en "x" new-x)
                   (js-set! en "y" new-y)))])))
