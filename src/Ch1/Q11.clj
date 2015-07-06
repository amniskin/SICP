;1.11
(defn f_11_1 [n]
  (cond (< n 3) n
        :else (+ 
          (f_11_1 (- n 1)) 
          (* 2 (f_11_1 (- n 2))) 
          (* 3 (f_11_1 (- n 3))))))
(defn f_11_2 [n]
  (defn f_11_2_iter [count first second third]
    (cond (= n count) third
      	  (< n 0) 0
      	  :else (f_11_2_iter (+ count 1) 
      	  	second 
          	third 
          	(+ (* first 3) (* 2 second) third) )))
  (cond (< n 3) n
    :else (f_11_2_iter 2 0 1 2)))

