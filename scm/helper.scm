;;===================( Helpers )==================;;
(define-macro (.. method obj . args)
  `(js-invoke ,obj ',method ,@args))

(define \> js-invocation)

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

(define-macro (define-generic name . memoize)
  (let ([mem (if (null? memoize) #f (car memoize))])
    `(define ,name (.. define_generic CLOS ,mem))))

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


(define-generic _num-pair->key #t) ;for memoization
(define-method _num-pair->key (x y)
  (string-append (number->string x) "," (number->string y)))
(define (num-pair->key x y)
  (js-call _num-pair->key x y))


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

(define (random-int n m)
  (+ (floor (* (\> ROT 'RNG '(getUniform)) n)) m))
(define (random-item vec)
  (vector-ref vec (random-int (vector-length vec) 0)))

;;set
(define (make-set) (make-eqv-hashtable))
(define (set . contents)
  (let ([aSet (make-set)])
    (for-each (lambda (c)
                (hashtable-set! aSet c #t)) contents)
    aSet))
(define (set-contains? s c)
  (hashtable-ref s c #f))
(define (set-add! s c)
  (hashtable-set! s c #t))
(define (set-remove! s c)
  (hashtable-set! s c #f))
(define set-size hashtable-size)
(define set-contents hashtable-keys)


(define-record-type (<rgb> make-rgb rgb?)
  (fields (immutable red rgb-red)
          (immutable green rgb-green)
          (immutable blue rgb-blue)))
(define (rgb->css-string c)
  (if (rgb? c)
      (string-append "rgb("
                     (number->string (rgb-red c)) ","
                     (number->string (rgb-green c)) ","
                     (number->string (rgb-blue c)) ")")
      c))
(define (random-close-color base var)
  (make-rgb (+ (rgb-red   base) (random-integer var))
            (+ (rgb-green base) (random-integer var))
            (+ (rgb-blue  base) (random-integer var))))

(define Math (js-eval "Math"))
(define Window (js-eval "window"))

(define-macro (timeout n body)
  `(.. setTimeout Window (js-lambda [] ,body) ,n))


(define (game-map-ref gmap x y default)
  (hashtable-ref gmap (num-pair->key x y) default))
(define (game-map-set! gmap x y val)
  (hashtable-set! gmap (num-pair->key x y) val))


(define (floodfill gameMap x y callback)
  (floodfill- gameMap x y (game-map-ref gameMap x y #f) callback))

(define (floodfill- gameMap x y val callback)
  (define (inner acc x y)
    (let ([key (num-pair->key x y)])
      (cond [(memv key acc) acc]
            [(eqv? val (game-map-ref gameMap x y #f))
             (callback  x y)
             (letrec ([a (inner (cons (num-pair->key x y) acc) (- x 1) y)]
                      [b (inner a (+ x 1) y)]
                      [c (inner b x (- y 1))]
                      [d (inner c x (+ y 1))])
               d)]
            [else
             (callback  x y)
             acc])))
  (inner '()  x y))

;;====================( End )=====================;;
