(defn null? [x] (= () x))
(defn accumulate [op initial sequen]
  (if (null? sequen) initial
    (op (first sequen)
        (accumulate op initial (rest sequen)))))


(defn count-leaves [t]
  (accumulate 
    + 
    0 
    (map #(if (seq? %) 
            (count-leaves %)
            1) 
         t)))



