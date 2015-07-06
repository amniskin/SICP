(defn null? [x] (= () x))
(defn accumulate [op initial sequen]
  (if (null? sequen) initial
    (op (first sequen)
        (accumulate op initial (rest sequen)))))


(defn maps [p things]
  (accumulate #(cons (p %1) %2) () things))
(defn append [seq1 seq2]
  (accumulate cons seq2 (reverse seq1)))
(defn length [things]
  (accumulate #(inc %2) 0 things))
