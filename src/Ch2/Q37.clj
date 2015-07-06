(defn null? [x] (= () x))
(defn accumulate [op initial sequen]
  (if (null? sequen) initial
    (op (first sequen)
        (accumulate op initial (rest sequen)))))

(defn accumulate-n [op init seqs]
  (if (null? (first seqs))
    nil
    (cons (accumulate op init (map first seqs))
          (accumulate-n op init (map rest seqs)))))


(defn dot-product [u v]
  (accumulate + 0 (map * u v)))
(defn matrix-*-vector [m v]
  (map #(dot-product % v) m))
(defn transpose [m]
  (accumulate-n #(cons %1 %2) () m))
(defn matrix-*-matrix [m n]
  (let [cols (transpose n)]
    (map #(matrix-*-vector cols %) m)))
