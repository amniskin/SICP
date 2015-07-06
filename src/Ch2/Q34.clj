(defn null? [x] (= () x))
(defn accumulate [op initial sequen]
  (if (null? sequen) initial
    (op (first sequen)
        (accumulate op initial (rest sequen)))))

(defn horner-eval [x coefficient-sequence]
  (accumulate #(+ %1 (* x %2)) 0 coefficient-sequence))
