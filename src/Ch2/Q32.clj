(defn null? [x] (= x ()))
(defn subsets [s]
  (if (null? s)
      (list ()) ;nil is different in clojure
      (let [remaining (subsets (rest s))
            thing     (first s)]
        (concat (map #(concat (list thing) %) remaining) remaining))))
