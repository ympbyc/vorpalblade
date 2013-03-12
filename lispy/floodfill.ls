(macro defun (name params rest...)
       (var ~name (function ~params ~rest...)))

;;map is {"Number,Number":Number} hashtable
;;x and y  indicates the starting point
;;returns an array of keys
(defun floodfill (map x y callback)
  (floodfill_dash map x y (map_ref map x y) callback))

(defun map_ref (map x y)
  (get (str x "," y)  map))

(defun floodfill_dash (map _x _y val callback)
  (var acc [])
  (defun rec (x y)
    (if (|| (!= (map_ref map x y) val)
         (> (-> acc (.indexOf (str x "," y))) -1))
        acc
      (do
          (acc.push (str x "," y))
          (callback x y)
          (rec (- x 1) y)
          (rec (+ x 1) y)
          (rec x (- y 1))
          (rec x (+ y 1))
          acc)))
  (rec _x _y))
