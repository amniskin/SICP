(defn null? [x] (= () x))
(defn accumulate [op initial sequen]
  (if (null? sequen) initial
    (op (first sequen)
        (accumulate op initial (rest sequen)))))
(defn flatmap [proc items]
  (accumulate append nil (map proc items)))

