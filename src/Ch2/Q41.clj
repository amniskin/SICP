(defn null? [x] (= () x))
(defn accumulate [op initial sequen]
  (if (null? sequen) initial
    (op (first sequen)
        (accumulate op initial (rest sequen)))))
(defn flatmap [proc items]
  (accumulate concat nil (map proc items)))
(defn unique-pairs [n]
  (defn make-pairs [i]
    (map #(list i (inc %)) (range i)))
  (flatmap #(make-pairs (inc %)) (range n)))
(defn unique-triples [n]
  (defn make-triples [pairs]
    (map #(list (first pairs) (nth pairs 1) (inc %)) (range (nth pairs 1))))
  (flatmap #(make-triples %) (unique-pairs n)))
(defn triples-less-than [n s]
  (filter #(= (reduce + %) s) 
          (unique-triples n)))


