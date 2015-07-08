(defn null? [x] (= () x))
(defn accumulate [op initial sequen]
  (if (null? sequen) initial
    (op (first sequen)
        (accumulate op initial (rest sequen)))))
(defn flatmap [proc items]
  (accumulate concat nil (map proc items)))

(defn make-pair-sum [pair]
  (let [one (first pair)
        two (nth pair 1)]
    (list one two (+ one two))))
(defn smallest-divisor [n]
  (defn find-divisor [m test]
    (cond (> (* test test) m) m
          (= (mod m test) 0) test
          :else (find-divisor m (+ test 1))))
  (find-divisor n 2))
(defn prime? [a]   ; if we need speed over accuracy we'd use Miller-Rabin
  (= a (smallest-divisor a)))
(defn prime-sum? [pair]
  (let [a (first pair)
        b (nth pair 1)]
    (prime? (+ a b))))


(defn unique-pairs [n]
  (defn make-pairs [i]
    (map #(list i %) (range 1 i)))
  (flatmap #(make-pairs (inc %)) (range n)))

(defn prime-sum-pairs [n]
  (map make-pair-sum
       (filter prime-sum?
               (unique-pairs n))))
