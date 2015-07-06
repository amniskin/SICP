(defn null? [x] (= x ()))
(defn make-mobile [left right]
  (list left right))
(defn make-branch [length structure]
  (list length structure))

;Part a: return the branches and components
(defn left-branch [mobile]
  (first mobile))
(defn right-branch [mobile]
  (first (rest mobile)))
(defn branch-length [branch]
  (first branch))
(defn branch-structure [branch]
  (first (rest branch)))


;Part b
(defn total-weight [mobile]
  (if (not (seq? mobile)) mobile
    (+ (total-weight (branch-structure (left-branch mobile)))
       (total-weight (branch-structure (right-branch mobile))))))

;Part C
(defn balanced? [mobile]
  (defn torque? [thing] 
    (= (* (branch-length (left-branch thing)) (total-weight (left-branch thing)))
       (* (branch-length (right-branch thing)) (total-weight (right-branch thing)))))
  (if (not (seq? mobile)) true
    (and (torque? mobile)
         (balanced? (branch-structure (left-branch mobile)))
         (balanced? (branch-structure (right-branch mobile))))))


;Part D
; This cannot be done in clojure because the cdr of a cons has to be
; a sequential object. But we'd just switch first and rest with car and
; cdr respectively in the definitions for left-branch, right-branch,
; branch-length, and branch-structure.

