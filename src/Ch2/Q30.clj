(defn null? [thing] (= thing ()))
(defn square [thing] (* thing thing))
(defn square-tree-map [tree]
  (map (fn [x] (if (seq? x) (square-tree-map x)
                 (square x)))
       tree))
(defn square-tree [tree]
  (cond (null? tree) nil
        (not (seq? tree)) (square tree)
        :else (concat (list (square-tree (first tree))) (square-tree (rest tree)))))

