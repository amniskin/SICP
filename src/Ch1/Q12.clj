;1.12
(defn pascal [row column]
  (cond (or (< row column) (< row 1) (< column 1)) 0
        (or (= row column) (= column 1)) 1
        :else (+ (pascal (- row 1) (- column 1))
                 (pascal (- row 1) column))))

