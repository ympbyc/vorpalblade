(define ROT (js-eval "ROT"))

(define (map-gen)
  (js-invocation ROT 'RNG
                 '(setSeed 123))
  (let ((aMap (js-new "ROT.Map.Digger"))
        (disp (js-new "ROT.Display" (js-obj "fontSize" 8))))
    (element-insert! "body" (js-invocation disp '(getContainer)));append canvas
    (js-invocation aMap `(create ,(js-ref disp 'DEBUG)))))

(map-gen)
