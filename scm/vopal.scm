(define-macro (.. method obj . args)
  `(js-invocation ,obj (,method ,@args)))

(define \> js-invocation)

(define ROT (js-eval "ROT"))

(define *GAME-display* (js-new "ROT.Display" (js-obj  "fontSize" 14 "width" 100 "height" 40)))
(define *GAME-freeCells* (make-vector 0))
(define *GAME-map* (js-obj))
(define *GAME-player* (js-obj))

(define KEYMAP (js-eval "({ 38: 0, 33: 1, 39: 2, 34: 3, 40: 4, 35: 5, 37: 6, 36: 7})"))

(define (digCallback x y wall)
  (let ((key (num-pair->key x y)))
    (if (= wall 0)
      (begin
        (vector-set! *GAME-freeCells* (vector-length *GAME-freeCells*) key)
        (js-set! *GAME-map* key "."))
      (js-set! *GAME-map* key "#"))))

(define (map-gen)
  (\> ROT
      'RNG
      '(setSeed 123))
  (let ((aMap (js-new "ROT.Map.Digger" 100 40))
        (disp *GAME-display*))
    (element-insert! "#rot-container" (.. getContainer disp));append canvas
    (.. create aMap digCallback)))

(define (draw-whole-map)
  (for-each (lambda (cell)
              (let ((c  (map string->number (string-split (car cell) ","))))
                (.. draw *GAME-display* (car c) (cadr c) (cdr cell))))
            (js-obj-to-alist *GAME-map*)))

(define (player-init)
  (let* ((index (floor (* (\> ROT 'RNG '(getUniform))
                          (vector-length *GAME-freeCells*))))
         (key (vector-ref (.. splice *GAME-freeCells* index 1) 0))
         (parts (string-split key ","))
         (x (string->number (car parts)))
         (y (string->number (cadr parts))))
    (set! *GAME-player* (js-obj "x" x "y" y))
    (player-draw *GAME-player*)))

(define (player-draw pl)
  (.. draw *GAME-display* (js-ref pl 'x)
                          (js-ref pl 'y)
                          "@"
                          "#ff0"))

(define (num-pair->key x y)
  (string-append (number->string x) "," (number->string y)))


(define (player-listen-key e pl)
  (let ((direction (js-ref KEYMAP (number->string (js-ref e 'keyCode)))))
    (if (not (js-undefined? direction))
          (let* ((diff (vector-ref (js-ref (js-ref ROT 'DIRS) "8") direction))
                 (cur-x (js-ref pl 'x))
                 (cur-y (js-ref pl 'y))
                 (new-x (+ cur-x (vector-ref diff 0)))
                 (new-y (+ cur-y (vector-ref diff 1)))
                 (new-key (num-pair->key new-x new-y)))
            (if  (and (not (js-undefined? (js-ref *GAME-map* new-key)))
                      (> (.. indexOf *GAME-freeCells* new-key) -1))
                 (begin
                   (.. draw *GAME-display* cur-x cur-y
                       (js-ref *GAME-map* (num-pair->key cur-x cur-y)))
                   (js-obj "x" new-x "y" new-y))
                  pl))
          pl)))


((lambda ()
   (map-gen)
   (draw-whole-map)
   (player-init)
   (add-handler! "body"
                 "keydown"
                 (lambda (e)
                   (set! *GAME-player* (player-listen-key e *GAME-player*))
                   (player-draw *GAME-player*)))))
