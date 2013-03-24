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
(define *map-width* 30)
(define *map-height* 20)
(define *seed* 1236)
(define *path-cache-duration* 10)
(define *visibility-distance* 3)
(define *GAME-display* (js-eval "Three.display"))
(define *GAME-engine* (js-new "ROT.Engine"))
(define *keymap* (construct-eq-hashtable
                  89 7    ;y
                  75 0    ;k
                  85 1    ;u
                  76 6      ;2    ;l

                  78 3    ;n
                  74 4    ;j
                  66 5    ;b
                  72 2))  ;h     ;6
(define *objects* (construct-eq-hashtable
                   'player #f
                   'enemies '()))
(define (game-player) (hashtable-ref *objects* 'player #f))
(define (game-enemies) (hashtable-ref *objects* 'enemies #f))

(define *light-pass-char* (set "." "~" "\""))
(define *creature-pass-char* (set "." "~" "+" "\""))


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
                           en)) (iota 0)))
  (.. start *GAME-engine*))


;;===================(  Main )====================;;
(timeout 2000
         #;(element-insert! "#rot-container" (.. getContainer *GAME-display*)) ;;add canvas to html
         (let-values ([[freeCells gameMap] (map-gen *seed*)])
           #;(draw-whole-map gameMap)
           (game-init gameMap freeCells)))
