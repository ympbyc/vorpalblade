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

(define (random-int n m)
  (+ (floor (* (\> ROT 'RNG '(getUniform)) n)) m))
(define (random-item vec)
  (vector-ref vec (random-int (vector-length vec) 0)))
;;====================( End )=====================;;
