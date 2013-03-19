;;====================( FOV )=====================;;
(define (light-passes? gameMap)
  (define-generic l-pass #t) ;memoize
  (define-method l-pass (x y)
    (let ([cell (game-map-ref gameMap x y "#")])
      (set-contains? *light-pass-char* cell)))
  l-pass)

(define memoized-light-passes #f)

(define *drawn-fov* (make-set))
(define *fov-redraw-everytime* (set "~" "+"))

(define *currently-lit* (make-set))

(define (draw-lit cur-lit next-lit gameMap)
  (set-for-each (lambda (key)
                  (let* ([x (key-x key)]
                         [y (key-y key)]
                         [ch (game-map-ref gameMap x y "#")])
                    (if (or (eqv? ch "~")
                            (not (set-contains? cur-lit key)))
                            (draw-colored-char
                             x y
                             ch))))
                next-lit))
(define (draw-darker cur-lit next-lit gameMap)
  (set-for-each (lambda (key)
                  (let ([x (key-x key)]
                        [y (key-y key)])
                    (unless (set-contains? next-lit key)
                            (draw-colored-char-darker
                             x y
                             (game-map-ref gameMap x y "#")))))
                cur-lit))


;responsible only for the dungeon.
(define (draw-fov gameMap freeCells pl-x pl-y)
  (or memoized-light-passes
      (set! memoized-light-passes (light-passes? gameMap)))
  (let ([key (num-pair->key pl-x pl-y)]
        [cell (game-map-ref gameMap pl-x pl-y #f)]
        [fov (js-new "ROT.FOV.PreciseShadowcasting" memoized-light-passes)]
        [next-lit (make-set)])
    (cond [(and (eqv? cell ".")
                (not (set-contains? *currently-lit* key)))
           (draw-floodfill gameMap pl-x pl-y)]
          [(eqv? cell ".") #f]
          [(or (set-contains? *fov-redraw-everytime* cell)
               (not (set-contains? *drawn-fov* key)))
           (set-add! *drawn-fov* key)
           (.. compute fov pl-x pl-y *visibility-distance*
               (js-lambda [x y r v]
                          (set-add! next-lit (num-pair->key x y))))
           (draw-lit *currently-lit* next-lit gameMap)
           (draw-darker *currently-lit* next-lit gameMap)
           (set! *currently-lit* next-lit)])))

(define (draw-floodfill gameMap pl-x pl-y)
  (floodfill gameMap pl-x pl-y
             (lambda [x y]
               (set-add! *currently-lit* (num-pair->key x y))
               (draw-colored-char
                x y
                (game-map-ref gameMap x y "#")))))



;;==================(  Visual )===================;;
(define *char-color* (construct-eq-hashtable
                      "#" (make-rgb 34 34 34)
                      "." (make-rgb 255 255 255)
                      "\"" (make-rgb 105 212 85)
                      "+" (make-rgb 245 113 37)
                      "~" (make-rgb 135 115 255))) ;blue
(define *char-bg-color* (construct-eq-hashtable
                         "#" (make-rgb 192 169 179)
                         "." (make-rgb 18 18 44)
                         "\"" (make-rgb 13 8 33)
                         "+" (make-rgb 117 38 18)
                         "~" (make-rgb 38 54 138))) ;light blue
(define (char-color ch)
  (hashtable-ref *char-color* ch (make-rgb 255 255 255)))
(define (char-bg-color ch)
  (hashtable-ref *char-bg-color* ch (make-rgb 255 255 255)))

(define *variate-color* (set "#" "~" "\""))
(define (variate-color? ch)
  (set-contains? *variate-color* ch))

(define (draw-colored-char x y ch)
  (let ([chc  (char-color ch)]
        [chbc (char-bg-color ch)])
    (if (and (variate-color? ch) (< (.. random Math) 0.2))
        (draw-colored-char-variation x y ch chc chbc)
        (draw-cell *GAME-display* x y ch
          (rgb->css-string chc)
          (rgb->css-string chbc)))))

(define (draw-colored-char-variation x y ch chc chbc)
  (timeout
   100
   (draw-cell *GAME-display* x y ch
       (rgb->css-string (random-close-color chc 40))
       (rgb->css-string (random-close-color chbc 40)))))

(define (draw-colored-char-darker x y ch)
  (let ([chc (char-color ch)]
        [chbc (char-bg-color ch)])
    (draw-cell *GAME-display* x y ch
               (rgb->css-string (darker-color chc))
               (rgb->css-string (darker-color chbc)))))

;;=====================( IO )=====================;;
(define (draw-whole-map gameMap)
  (vector-for-each/key (lambda [row y]
    (vector-for-each/key (lambda [chr x]
      (draw-colored-char x y chr)) row))
     gameMap))

(define-generic creature-draw)
(define-method  creature-draw ([pl <player>])
  (draw-cell *GAME-display*
      (creature-x pl)
      (creature-y pl)
      "@"
      "#e0baf6"
      "#000"))
(define-method  creature-draw ([en <enemy>])
  (draw-cell *GAME-display*
      (creature-x en)
      (creature-y en)
      "f"
      "#f57125"
      "#000"))
