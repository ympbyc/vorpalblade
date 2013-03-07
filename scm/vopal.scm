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

(define (add-handler-once! sel ev f)
  (define (handler e)
    (remove-handler! sel ev handler)
    (f e))
  (add-handler! sel ev handler))

(define-macro (define-generic name)
  `(define ,name (.. define_generic CLOS)))
(define-macro (define-method gener argspec . body)
  (let ([args (fold-right (lambda (x acc)
                            (if (pair? x)
                                (cons (cons (cadr x) (car acc))
                                      (cons (car x) (cdr acc)))
                                (cons (cons (js-eval "undefined") (car acc))
                                      (cons x (cdr acc))))) '(() . ()) argspec)])
    `(.. define_method CLOS
         ,gener
         ,(list->vector (car args))
         (js-closure (lambda ,(cdr args) ,@body)))))

(define-macro (define-class name . parents)
  `(define ,name (.. define_class CLOS ,(list->vector parents))))

(define (make class obj)
  (.. make CLOS class obj))

(define CLOS (js-eval "CLOS"))
;;====================( End )=====================;;

(define-class animal)
(define-generic talk)
(console-log animal)
(console-log (macroexpand '(define-method talk ((a animal))
  (string-append (js-ref a 'name) " said something"))))
(define-method talk ((a ,animal))
  (string-append (js-ref a 'name) " said something"))
(display (talk (make animal (js-obj "name" "Sammy"))))

;;===================( Config )===================;;
(define *map-width* 60)
(define *map-height* 24)
(define *GAME-display* (js-new "ROT.Display" (js-obj  "fontSize" 13 "fontFamily" "Osaka" "width" *map-width* "height" *map-height*)))
(define *GAME-engine* (js-new "ROT.Engine"))
(define *keymap* (construct-eq-hashtable
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
  (let ([aMap (js-new "ROT.Map.Digger" *map-width* *map-height*)]
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
;;player has to be represented as a js object due to the lib we are using
(define (make-player x y gameMap freeCells)
  (letrec ([player (js-obj
                    "x" x
                    "y" y
                    "getSpeed" (js-closure (lambda () 100))
                    "act" (js-closure (lambda ()
                                        (.. lock *GAME-engine*)
                                        (add-handler-once!
                                         "body"
                                         "keydown"
                                         (lambda [e]
                                           (player-movement e player gameMap freeCells)
                                           (player-draw player))))))])
    player))

(define (player-x pl)
  (js-ref pl 'x))
(define (player-y pl)
  (js-ref pl 'y))
;;====================( End )=====================;;


;;=============( Player Generation )==============;;
;;clean and functional
(define (player-init gameMap freeCells)
  (let* ([index (floor (* (\> ROT 'RNG '(getUniform))
                          (vector-length freeCells)))]
         [key (vector-ref freeCells index)]
         [parts (string-split key ",")]
         [x (string->number (car parts))]
         [y (string->number (cadr parts))])
    (make-player x y gameMap freeCells)))
;;====================( End )=====================;;


;;==============( Player Movement )===============;;
;;Draws to the map
;;mutates player object
(define (player-movement e pl gameMap freeCells)
  (let ([direction (hashtable-ref *keymap* (js-ref e 'keyCode) #f)])
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
                 (js-set! pl "x" new-x)
                 (js-set! pl "y" new-y))
               pl))
        pl)))
;;====================( End )=====================;;


;;================( Game Engine )=================;;
(define (game-init pl)
  (.. addActor *GAME-engine* pl)
  (.. start *GAME-engine*))
;;====================( End )=====================;;

;;===================(  Main )====================;;
((lambda []
   (element-insert! "#rot-container" (.. getContainer *GAME-display*)) ;;add canvas to html
   (let-values ([[freeCells gameMap] (map-gen 24)])
     (draw-whole-map gameMap)
     (let ([player (player-init gameMap freeCells)])
       (player-draw player)
       (game-init player)))))
;;====================( End )=====================;;
