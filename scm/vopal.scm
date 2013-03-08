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
  (letrec ([handler (add-handler! sel ev (lambda (e)
                                           (remove-handler! sel ev handler)
                                           (f e)))])))

(define-macro (js-lambda args . body)
  `(js-closure (lambda ,args ,@body)))

(define-macro (define-generic name)
  `(define ,name (.. define_generic CLOS)))

(define-macro (define-method gener argspec . body)
  (let ([args (fold-right (lambda (x acc)
                            (if (pair? x)
                                (cons (cons (cadr x) (car acc))
                                      (cons (car x) (cdr acc)))
                                (cons (cons '(js-eval "undefined") (car acc))
                                      (cons x (cdr acc))))) '(() . ()) argspec)])
    `(.. define_method CLOS
         ,gener
         (list->vector (map eval ',(car args)))
         (js-closure (lambda ,(cdr args) ,@body)))))

(define-macro (define-class name parents . fn)
  (if (null? fn)
      `(define ,name (.. define_class CLOS (list->vector ,parents)))
      `(define ,name (.. define_class CLOS (list->vector ,parents) (js-closure ,(car fn))))))

(define (make class obj)
  (.. make CLOS class obj))

(define (clos-slot-exists x key typ)
  (.. slot_exists CLOS key typ))

(define CLOS (js-eval "CLOS"))



(define (vector->stream vec)
  (define (aux i)
    (if (= i (vector-length vec))
        '()
        (lambda () (cons (vector-ref vec i) (aux (+ i 1))))))
  (aux 0))

(define (stream-contains? strm x)
  (define (aux strm)
    (if (null? strm) #f
        (let ([ss (strm)])
          (or (eqv? (car ss) x)
              (aux (cdr ss))))))
  (aux strm))
;;====================( End )=====================;;


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
;;====================( End )=====================;;


;;=====================( IO )=====================;;
(define (draw-whole-map gameMap)
  (vector-for-each (lambda [key]
                     (let ([c  (map string->number (string-split key ","))])
                       (.. draw *GAME-display* (car c) (cadr c) (hashtable-ref gameMap key "#"))))
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
;;====================( End )=====================;;


;;=================( Creatures )==================;;
(define (make-creature <class> x y gameMap freeCells)
  (letrec ([creature (make <class>
                       (js-obj
                        "x"        x
                        "y"        y
                        "getSpeed" (js-lambda [] (js-call get-speed creature))
                        "act"      (js-lambda [] (js-call rot-act creature gameMap freeCells))))])
    creature))
;;====================( End )=====================;;


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
                       (js-call creature-draw pl)
                       (.. unlock *GAME-engine*))))

(define-method rot-act ([en <enemy>] gameMap freeCells)
  (define passbale-callback (js-lambda [x y]
     (eqv? "." (hashtable-ref gameMap (num-pair->key x y) #f))))
  (define (path-callback path) (js-lambda [x y]
                                          (.. push path (vector x y))))
  (let* ([x 4] ;;pl-x
         [y 6]
         [cur-x (creature-x en)]
         [cur-y (creature-y en)]
         [path (make-vector 0)]
         [astar (js-new "ROT.Path.AStar" x y passbale-callback (js-obj "topology" 4))])
    (.. compute astar cur-x cur-y (path-callback path))
    (.. shift path) ;remove current position
    (cond [(= (\> path 'length) 1)
           (.. lock *GAME-engine*)
           (display "GAME OVER")]
          [else
           (let ([new-x (vector-ref (vector-ref path 0) 0)]
                 [new-y (vector-ref (vector-ref path 0) 1)])
             (.. draw *GAME-display* cur-x cur-y (hashtable-ref gameMap (num-pair->key cur-x cur-y) "."))
             (js-set! en "x" new-x)
             (js-set! en "y" new-y)
             (js-call creature-draw en))])))
;;====================( End )=====================;;


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
;;====================( End )=====================;;


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
               (begin
                 (.. draw *GAME-display* cur-x cur-y
                     (hashtable-ref gameMap (num-pair->key cur-x cur-y) "."))
                 (js-set! pl "x" new-x)
                 (js-set! pl "y" new-y))
               pl))
        pl)))
;;====================( End )=====================;;


;;================( Game Engine )=================;;
(define (game-init gameMap freeCells)
  (let ([pl (creature-init <player> gameMap freeCells)])
    (.. addActor *GAME-engine* pl)
    (js-call creature-draw pl))
  (for-each (lambda (_)
              (let ([en (creature-init <enemy> gameMap freeCells)])
                (.. addActor *GAME-engine* en)
                (js-call creature-draw en))) (iota 5))
  (.. start *GAME-engine*))
;;====================( End )=====================;;


;;===================(  Main )====================;;
((lambda []
   (element-insert! "#rot-container" (.. getContainer *GAME-display*)) ;;add canvas to html
   (let-values ([[freeCells gameMap] (map-gen 24)])
     (draw-whole-map gameMap)
     (game-init gameMap freeCells))))
;;====================( End )=====================;;
