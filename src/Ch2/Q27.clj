(defn null? [thing] (= () thing))
(defn deep-reverse [things]
  (cond (and (seq? things) (not (null? things)))
    (concat (deep-reverse (rest things)) (list (deep-reverse (first things))))
    (and (seq? things) (null? things))
    nil
    :else things)) 
