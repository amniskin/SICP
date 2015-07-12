(defn null? [x] (= () x))
(defn accumulate [op initial sequen]
  (if (null? sequen) initial
    (op (first sequen)
        (accumulate op initial (rest sequen)))))
(defn flatmap [proc items]
  (accumulate concat nil (map proc items)))
(defn unique-pairs [n]
  (defn make-pairs [i]
    (map #(list i %) (range 1 i)))
  (flatmap #(make-pairs (inc %)) (range n)))
(defn make-pairs-1-n [i n]
  (map #(list i %) (range 1 (inc n))))
(defn add-new-pairs [items i n]
  (flatmap #(concat items %) (make-pairs-1-n i n)))
(defn in-check? [one two]
  (or (= (first one) (first two)) ;same column
      (= (second one) (second two)) ;same row
      (= (+ (first one) (second one))
         (+ (first two) (second two))) ;downward diagonal
      (= (- (first one) (second one))
         (- (first two) (second two))))) ;upward diagonal
(defn good-to-go? [items]
  (accumulate #(and %1 %2) true (map #(not (in-check? (first items) %)) (rest items))))
(defn make-em [items i n]
  (map #(concat (list %) items) (make-pairs-1-n i n)))
(defn n-queens-solutions [n]
  (defn make-em [items i]
    (map #(concat (list %) items) (make-pairs-1-n i n)))
  (defn doit [items i]
    (if (> i n) items
        (doit (filter #(good-to-go? %) (flatmap #(make-em % i) items)) (inc i))))
  (doit (make-em () 1) 2))

