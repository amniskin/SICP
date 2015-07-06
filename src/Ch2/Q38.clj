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


;The operation should be associative

