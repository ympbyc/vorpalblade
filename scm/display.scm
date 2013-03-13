;;====================( FOV )=====================;;
(define (light-passes? gameMap)
  (define-generic l-pass #t) ;memoize
  (define-method l-pass (x y)
    (let ([cell (game-map-ref gameMap x y "#")])
      (set-contains? *light-pass-char* cell)))
  l-pass)

(define memoized-light-passes #f)

(define *lit-floor* (make-set))
(define *drawn-fov* (make-set))
(define *fov-redraw-everytime* (set "~"))

;responsible only for the dungeon.
(define (draw-fov gameMap freeCells pl-x pl-y)
  (or memoized-light-passes
      (set! memoized-light-passes (light-passes? gameMap)))
  (let ([key (num-pair->key pl-x pl-y)]
        [cell (game-map-ref gameMap pl-x pl-y #f)])
    (when (and (eqv? cell ".")
               (not (set-contains? *lit-floor* key)))
          (draw-floodfill gameMap pl-x pl-y))
    (when (or (set-contains? *fov-redraw-everytime* cell)
              (not (set-contains? *drawn-fov* key)))
          (let ([fov (js-new "ROT.FOV.PreciseShadowcasting" memoized-light-passes)])
            (.. compute fov pl-x pl-y *visibility-distance*
                (js-lambda
                 (x y r v)
                 (set-add! *drawn-fov* (num-pair->key x y))
                 (draw-colored-char
                  x y
                  (game-map-ref gameMap x y "#"))))))))

(define (draw-floodfill gameMap pl-x pl-y)
  (floodfill gameMap pl-x pl-y
             (lambda [x y]
               (set-add! *lit-floor* (num-pair->key x y))
               (draw-colored-char
                x y
                (game-map-ref gameMap x y "#")))))



;;==================(  Visual )===================;;
(define *char-color* (construct-eq-hashtable
                      "#" "#222"
                      "." "#fff"
                      "\"" (make-rgb 105 212 85)
                      "+" "#f57125"
                      "~" (make-rgb 135 115 255))) ;blue
(define *char-bg-color* (construct-eq-hashtable
                         "#" "#c0a9b3"
                         "." "#12122c"
                         "\"" (make-rgb 13 8 33)
                         "+" "#752612"
                         "~" (make-rgb 38 54 138))) ;light blue
(define (char-color ch)
  (hashtable-ref *char-color* ch "#fff"))
(define (char-bg-color ch)
  (hashtable-ref *char-bg-color* ch "#fff"))

(define (draw-colored-char x y ch)
  (let ([chc  (char-color ch)]
        [chbc (char-bg-color ch)])
    (if (and (rgb? chc) (< (.. random Math) 0.2))
        (draw-colored-char-variation x y ch chc chbc)
        (.. draw *GAME-display* x y ch
          (rgb->css-string chc)
          (rgb->css-string chbc)))))

(define (draw-colored-char-variation x y ch chc chbc)
  (timeout
   100
   (.. draw *GAME-display* x y ch
       (rgb->css-string (random-close-color chc 40))
       (rgb->css-string (random-close-color chbc 40)))))

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
      "#e0baf6"))
(define-method  creature-draw ([en <enemy>])
  (.. draw *GAME-display*
      (creature-x en)
      (creature-y en)
      "f"
      "#f57125"))
