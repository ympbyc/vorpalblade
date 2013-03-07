;;;;        You found a vorpal blade!
;;;;
;;;;              ############
;;;;              #..........#
;;;;              #....@.....#
;;;;              #.......>..#
;;;;              ############
;;;;
;;;; ympbyc the Programmer    Chaotic human hacker


;;===================( Helpers )==================;;
(define-macro (.. method obj . args)
  `(js-invoke ,obj ',method ,@args))

(define \> js-invocation)

(define (num-pair->key x y)
  (string-append (number->string x) "," (number->string y)))

(define (defined? x) (not (js-undefined? x)))

(define ROT (js-eval "ROT"))

(define (construct-eq-hashtable . contents)
  (define (set-fields! ht contents)
    (if (null? contents) ht
        (begin (hashtable-set! ht (car contents) (cadr contents))
               (set-fields! ht (cddr contents)))))
  (let ([size (length contents)])
    (if (= 0 (mod size 2))
        (set-fields! (make-eq-hashtable (/ size 2)) contents)
        (raise "construct-eq-hashtable: arguments must be in the form 'key val key val ...'"))))
;;====================( End )=====================;;


;;===================( Config )===================;;
(define *GAME-display* (js-new "ROT.Display" (js-obj  "fontSize" 14 "width" 100 "height" 40)))
(define KEYMAP (construct-eq-hashtable
                38 0
                33 1
                39 2
                34 3
                40 4
                35 5
                37 6
                36 7))
;;====================( End )=====================;;


;;===============( Map Generation )===============;;
;;Although some mutations are performed, CLEAN AND FUNCTIONAL as a whole.

(define (digCallback freeCells gameMap)
  (lambda [x y wall]
    (let ([key (num-pair->key x y)])
      (if (= wall 0)
          (begin
            (vector-set! freeCells (vector-length freeCells) key)
            (hashtable-set! gameMap key "."))
          (hashtable-set! gameMap key "#")))))

(define (map-gen seed)
  (\> ROT
      'RNG
      `(setSeed ,seed))
  (let ([aMap (js-new "ROT.Map.Digger" 100 40)]
        [freeCells (make-vector 0)]
        [gameMap  (make-eq-hashtable)])
    (.. create aMap (js-closure (digCallback freeCells gameMap)))
    (values freeCells gameMap)))
;;====================( End )=====================;;



;;=====================( IO )=====================;;
(define (draw-whole-map gameMap)
  (vector-for-each (lambda [key]
                     (let ([c  (map string->number (string-split key ","))])
                       (.. draw *GAME-display* (car c) (cadr c) (hashtable-ref gameMap key "#"))))
                   (hashtable-keys gameMap)))

(define (player-draw pl)
  (.. draw *GAME-display*
      (player-x pl)
      (player-y pl)
      "@"
      "#ff0"))
;;====================( End )=====================;;


;;==============( Player Datatype )===============;;
(define-record-type (player make-player player?)
  (fields [immutable x player-x]
          [immutable y player-y]))
;;====================( End )=====================;;


;;=============( Player Generation )==============;;
;;clean and functional
(define (player-init freeCells)
  (let* ([index (floor (* (\> ROT 'RNG '(getUniform))
                          (vector-length freeCells)))]
         [key (vector-ref freeCells index)]
         [parts (string-split key ",")]
         [x (string->number (car parts))]
         [y (string->number (cadr parts))])
    (make-player x y)))
;;====================( End )=====================;;

;;==============( Player Movement )===============;;
;;Draws map -- ok
;;returns a new player object
(define (player-movement e pl gameMap freeCells)
  (let ([direction (hashtable-ref KEYMAP (js-ref e 'keyCode) #f)])
    (if direction
        (let* ([diff (vector-ref (js-ref (js-ref ROT 'DIRS) "8") direction)]
               [cur-x (player-x pl)]
               [cur-y (player-y pl)]
               [new-x (+ cur-x (vector-ref diff 0))]
               [new-y (+ cur-y (vector-ref diff 1))]
               [new-key (num-pair->key new-x new-y)])
          (if  (and (hashtable-ref gameMap new-key #f)
                    (> (.. indexOf freeCells new-key) -1))
               (begin
                 (.. draw *GAME-display* cur-x cur-y
                     (hashtable-ref gameMap (num-pair->key cur-x cur-y) "."))
                 (make-player new-x new-y))
               pl))
        pl)))
;;====================( End )=====================;;


;;===================(  Main )====================;;
((lambda []
   (element-insert! "#rot-container" (.. getContainer *GAME-display*)) ;;add canvas to html
   (let-values ([[freeCells gameMap] (map-gen 1234)])
     (draw-whole-map gameMap)
     (let ([player (player-init freeCells)])
       (player-draw player)
       (add-handler! "body"
                     "keydown"
                     (lambda [e]
                       (set! player (player-movement e player gameMap freeCells))
                       (player-draw player)))))))
;;====================( End )=====================;;
