(defn null? [x] (= x ()))
(defn square [x] (* x x))
(defn tree-map [procedure tree]
  (map (fn [x] (if (seq? x) (tree-map procedure x)
                 (procedure x)))
       tree))
(defn square-tree [tree]
  (tree-map square tree))
