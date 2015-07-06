(defn null? [thing] (= () thing))
(defn fringe [items]
  (defn doit [things so-far]
    (cond (and (seq? things) (not (null? things)))
          (if (seq? (first things))
              (doit (concat (first things) (rest things)) so-far)
              (doit (rest things) (concat so-far (list (first things)))))
          (and (seq? things) (null? things))
          so-far
          (not (seq? things))
          (throw (Exception. "Please input a list to fringify"))))
  (doit items ()))

(defn deepreverse [x]
  (reduce conj () (map #(if (coll? %) (deepreverse %) %) x)))

(defn deepreverse [x]
  (reduce #(conj %1 (if (coll? %2) (deepreverse %2) %2)) () x))

(def deepreverse
  (partial reduce #(conj %1 (if (coll? %2) (deepreverse %2) %2)) ()))
