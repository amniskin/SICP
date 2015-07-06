(defn null? [x] (= x ()))
(defn fold-left [op init things]
  (defn iter [result leftover]
    (if (null? leftover)
      result
      (iter (op result (first leftover))
            (rest leftover))))
  (iter init things))
(defn fold-right [op init things]
  (if (null? things)
    init
    (op (first things)
        (fold-right op init (rest things)))))

(defn reverse-right [things]
  (fold-right #(concat %2 (list %1)) nil things))

(defn reverse-left [things]
  (fold-left #(cons %2 %1) nil things))
