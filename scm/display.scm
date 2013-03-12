;;====================( FOV )=====================;;
(define (light-passes? gameMap)
  (define-generic l-pass #t) ;memoize
  (define-method l-pass (x y)
    (let ([cell (game-map-ref gameMap x y "#")])
      (set-contains? *light-pass-char* cell)))
  l-pass)

(define memoized-light-passes #f)

;responsible only for the dungeon.
(define (draw-fov gameMap freeCells pl-x pl-y)
  (if (eqv? (game-map-ref gameMap pl-x pl-y #f) ".")
      (draw-floodfill gameMap pl-x pl-y)
      (or memoized-light-passes
          (set! memoized-light-passes (light-passes? gameMap)))
      (let ([fov (js-new "ROT.FOV.PreciseShadowcasting" memoized-light-passes)])
        (.. compute fov pl-x pl-y *visibility-distance*
            (js-lambda
             (x y r v)
             (draw-colored-char
              x y
              (game-map-ref gameMap x y "#")))))))

(define (draw-floodfill gameMap pl-x pl-y)
  (display pl-x) (display pl-y) (display (game-map-ref gameMap pl-x pl-y "*"))  (newline)
  (js-call floodfill gameMap pl-x pl-y
           (js-lambda [x y]
                      (console-log x)
                      (draw-colored-char
                      x y
                      (game-map-ref gameMap x y "#")))))



;;==================(  Visual )===================;;
(define *char-color* (construct-eq-hashtable
                      "#" "#222"
                      "." "#fff"
                      "\"" "#69d455"
                      "+" "#f57125"
                      "~" (make-rgb 135 115 255))) ;blue
(define *char-bg-color* (construct-eq-hashtable
                         "#" "#c0a9b3"
                         "." "#12122c"
                         "\"" "#12122c"
                         "+" "#752612"
                         "~" (make-rgb 38 54 138))) ;light blue
(define (char-color ch)
  (hashtable-ref *char-color* ch "#fff"))
(define (char-bg-color ch)
  (hashtable-ref *char-bg-color* ch "#fff"))

(define (draw-colored-char x y ch)
  (if (and (eqv? ch "~") (< (.. random Math) 0.2))
      (draw-colored-char-variation x y ch) ;;;;;to-enhance
      (.. draw *GAME-display* x y ch
          (rgb->css-string (char-color ch))
          (rgb->css-string (char-bg-color ch)))))
(define (draw-colored-char-variation x y ch)
  (timeout
   100
   (.. draw *GAME-display* x y ch
       (rgb->css-string (random-close-color (char-color ch) 40))
       (rgb->css-string (random-close-color (char-bg-color ch) 40)))))

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
