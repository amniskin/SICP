; To load a file in the terminal, type "scheme --load filename"
; So, to load this file, type "scheme -- load SICP.scm"

;First some useful global functions
(defn square [x] (* x x))
(defn cube [x] (* x x x))
(defn abs [x] (max x (- x)))


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

;1.12
(defn pascal [row column]
  (cond (or (< row column) (< row 1) (< column 1)) 0
        (or (= row column) (= column 1)) 1
        :else (+ (pascal (- row 1) (- column 1))
                 (pascal (- row 1) column))))

;1.13 -- Proof not included --

;1.14 The order is theta(2^n) for both computation and space because with
;       every added penny to the argument, you get two branches in the
;       computation. One branch is the exact computation done without the
;       added penny, and the other is similar enough to the first that we
;       can just consider it to be the same. Hence the number of steps
;       and the amount of space required is roughly doubled with every added
;       penny. (All of this is assuming that n >> 50, where 50 is the
;       largest of the coin sizes.)

;1.15 a) To figure out how many times this sin function must iterate (not
; even including the branching of the recursion), we need to find for what n
; is 12.15/3^n <= 0.1, so we let n be the ceiling of (ln(121.5)/ln(3)) 
; which is 4.37, so our n = 5.
; Next, we see for each iteration of p, how many times is sine called? Two.
; So our answer seems to be 2^5=32.
; b) Since Sine only calls p, and every time p is called, it preforms 5 
; operations (considering cube to be two operations), we can say that the
; number of steps required to compute sine(a) is approximately,
; 25^{ln10/ln3}*25^{ln|a|/ln3}, so I'd say the order is 
; theta(25^{ln|a|/ln3}) for the number of steps. For the space required,
; I think it would be the same because it can't throw out any of that data
; (even the 3 and the 4 from inside p) yet.

;1.16
(defn expt [b n]
  (defn expt-iter [b n a]
    (cond (= n 0) a
          (= (mod n 2) 0) (expt-iter (* b b) (/ n 2) a)
          :else (expt-iter b (- n 1) (* b a))))
  (expt-iter b n 1))

;1.17
(defn mult [a b]
  (defn halve [b] (/ b 2))
  (defn twice [a] (+ a a))
  (cond (= b 0) 0
        (= (mod b 2) 0) (mult (twice a) (halve b))
        :else (+ a (mult a (- b 1)))))

;1.18
(defn multi [a b]
  (defn halve [b] (/ b 2))
  (defn twice [a] (+ a a))
  (defn multi-iter [a b c]
    (cond (= b 0) c
          (= (mod b 2) 0) (multi-iter (twice a) (halve b) c)
          :else (multi-iter a (- b 1) (+ c a))))
  (multi-iter a b 0))

;1.19
(defn fib [n]
  (defn fib-iter [a b p q count]
    (cond (= count 0) b
          (= (mod count 2) 0)
            (fib-iter a b (+ (* p p) (* q q)) (* q (+ q p p)) (/ count 2))
          :else (fib-iter (+ (* b q) (* a q) (* a p))
                          (+ (* b p) (* a q))
                          p
                          q
                          (- count 1))))
  (fib-iter 1 0 0 1 n))

;1.2.5 Euclid's algorithm
(defn GCD [a b]
  (if (= b 0)
      a
      (GCD b (mod a b))))

;1.20
;(GCD 206 40) = (GCD 40 (mod 206 40))
;             = (GCD 40 6)
;             = (GCD 6 (mod 40 6))
;             = (GCD 6 4)
;             = (GCD 4 (mod 6 4))
;             = (GCD 4 2)
;             = (GCD 2 (mod 4 2))
;             = (GCD 2 0)
;             = 2
;   So, for applicative order evaluation, only 4 mod operations are
;     actually preformed.
;   For normal order, the process would look like this:
; (GCD 206 40) -> (= 40 0)? no ->
; (GCD 40 (mod 206 40)) -> (= (mod 206 40) 0)? (first one) no ->
; (GCD (mod 206 40) (mod 40 (mod 206 40))) 
;                             -> (= (mod 40 (mod 206 40)) 0)?
; and so on.



;1.21
(defn smallest-divisor [n]
  (defn find-divisor [m test]
    (cond (> (* test test) m) m
          (= (mod m test) 0) test
          :else (find-divisor m (+ test 1))))
  (find-divisor n 2))

(defn prime? [n]
  (if (= (smallest-divisor n) n)
  	true
  	false ))



;1.22
(defn search-for-primes [smallest number]
  (defn timed-prime-test [n]
    (time (prime? n)))
  (defn iter [l m]
    (cond (= m 0) (println "Done")
      (prime? l) (and (timed-prime-test l) (iter (+ l 1) (- m 1)))
      :else (iter (+ l 1) m)))
  (iter smallest number))
;The increases are only noticeable at much higher input sizes, but
; the increase in computation time is roughly by a factor of sqrt(10)
; for every factor of ten increase in the argument size.





;1.23 --- Done in scheme



;1.24 -- Done in scheme



;1.25
; Nope. Multiplying larger numbers is harder than multiplying smaller
; numbers. So computing the multiplication modulo m each time is better.



;1.26
; With applicative order evaluation, hers computes (expmod base ...) once 
; each iteration, while his computes it twice (not realizing the redundancy).
; So, for every time her program computes expmod, his branches off into two
; branches. So first we observe that theta(log_a(n))=theta(log_b(n)) by
; noting that log_a(n)=log_b(n)/log_b(a), so that
; theta(log_a(n))=theta(log_b(n)). Next we see that his
; theta(2^log(n))=theta(2^log_2(n))=theta(n).



;1.27
; They'll pass unless the random number generator happens to pick up
; a number that shares a common factor with n (but those are comparatively
; few, so it'll pass more times than not). That does, however, narrow the 
; search to a list of prime numbers and Carmichael numbers, and Carmichael
; numbers have their own properties, so that might help.




;1.28
; The statement is not true. For instance, there are no non-trivial roots
; of 1 in mod 9. -- Done in scheme



;1.29
(defn integral [f a b n]
  (defn sum [g u increment v]
    (cond (> u v) 0
      :else (+ (g u) (sum g (increment u) increment v))))
  (defn h []
    (cond (= (mod n 2) 0) (/ (- b a) n)
      :else (/ (- b a) (+ n 1))))
  (defn increase [b] (+ b (* 2 h)))
  (* (/ h 3) (+ 
    (f a) (f b) 
    (* 4 (sum f (+ a h) increase b)) 
    (* 2 (sum f (+ a (* 2 h)) increase b)))))




;1.30
(defn sum [term a next b]
  (defn iter [a result]
    (if (> a b)
      result
      (iter (next a) (+ (term a) result))))
  (iter a 0))




;1.31
(defn iterative-product [term a next b]
  (defn iter [a result]
    (if (> a b)
      result
      (iter (next a) (* (term a) result))))
  (iter a 1))


(defn recursive-product [term a next b]
  (cond (> a b) 1
    :else (+ (term a) (recursive-product term (next a) next b))))



;1.32
(defn accumulate [combiner null-value term a next-term b]
  (cond (> a b) null-value
    :else (combiner (term a) 
      (accumulate combiner null-value term 
        (next-term a) next-term b))))
(defn iterative-accumulate [combiner null-value term a next-term b]
  (defn iter [a result]
    (if (> a b)
      result
      (iter (next-term a) (combiner (term a) result))))
  (iter a null-value))



;1.33
(defn filtered-accumulate [combiner null-value term a next-term b filter]
  (cond (> a b) null-value
    (filter a) (combiner (term a)
      (filtered-accumulate combiner null-value term 
        (next-term a) next-term b filter))
    :else (filtered-accumulate combiner null-value term 
        (next-term a) next-term b filter)))



;1.34
; Since 2 is not a function, it does not compute.



;1.35
(defn fixed-point [f guess tolerance]
  (defn close-enough? [x y]
    (< (abs (- x y)) tolerance))
  (defn try-it [guess]
    (let [next-term (/ (+ guess (f guess)) 2)]
      (if (close-enough? guess (f guess))
        next-term
      (try-it next-term))))
  (try-it guess))
(defn sqrt [x]
	(fixed-point (fn [y] (/ (+ y (/ x y)) 2)) 1 0.001))




;1.36
(defn fixed-point-print [f guess tolerance]
  (defn close-enough? [x y]
    (< (abs (- x y)) tolerance))
  (defn try-it [guess]
    (let [next (/ (+ guess (f guess)) 2)]
      (if (close-enough? guess (f guess))
        (and (newline)
         (println next))
      (and (newline) (println guess) (try-it next)))))
  (try-it guess))

(defn nodamp-fixed-point [f guess tolerance]
  (defn close-enough? [x y]
    (< (abs (- x y)) tolerance))
  (defn try-it [guess]
    (let [next-term (f guess)]
      (if (close-enough? guess next-term)
        (and (newline)
         (println next-term))
      (and (newline) (println guess) (try-it next-term)))))
  (try-it guess))



;1.37
(defn cont-frac [n d k]
  (defn thing [n d a]
    (cond (= a k) (d a)
          :else (/ (n a) (+ (d a) (thing n d (inc a))))))
     (thing n d 1))
(defn cont-frac-iter [n d k]
  (defn build [i]
    (cond (= i k) (/ (n i) (d i))
          (< i k) (/ (n i) (+ (d i) (build (inc i))))
          :else false))
  (build 1))



;1.38
(defn n-1_38 [i] 1)
(defn d-1_38 [i]
  (cond (= (mod i 3) 2) (let [n (/ (inc i) 3)] (* 2 n))
  :else 1))
(defn e-1_38 [k]
  (+ (cont-frac n-1_38 d-1_38 k) 2))


;1.39
(defn tan-cf [x k]
  (if (= x 0)
    0
    (/ (cont-frac
          (fn [y] (-(square x))) 
          (fn [y] (- (* 2 y) 1))
          k) (- x))))



(defn average-damp [f]
  (fn [x] (/ (+ x (f x)) 2)))



;1.40
(defn deriv [g]
  (let [dx 0.00001]
    (fn [x]
      (/ (- (g (+ x dx)) (g x)) dx))))
(defn newton-transform [g]
  (fn [x] (- x (/ (g x) ((deriv g) x)))))
(defn fixed-point-of-transform [g transform guess]
  (fixed-point (transform g) guess 0.0001))
(defn cubic [a b c]
  (fn [x] (+ (cube x) (* a (square x)) (* b x) c)))


;1.41
(defn double-it [f]
  (fn [x] (f (f x))))




;1.42
(defn compose [f g]
  (fn [x] (f (g x))))


;1.43 **************************
(defn repeated [f n]
  (fn [x] 
    (defn thing [a]
      (if (= a 1)
        (f x)
        (f (thing (- a 1)))))
    (thing n)))

;1.44
(defn smooth [f]
  (fn [x]
    (let [dx 0.0001]
      (/ (+ (f (- x dx)) (f x) (f (+ x dx))) 3))))
(defn n-fold-smooth [f n]
  (fn [x]
    (((repeated smooth n) f) x)))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;; Chapter 2 ;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;2.1
(defn make-rat [n d]
	(let [g (GCD (abs n) (abs d))]
		(cond (> d 0) [(/ n g) (/ d g)]
			(< d 0) [(/ (- n) g) (/ (- d) g)])))


;2.2
(defn make-segment [a b] [a b])
(defn start-segment [l] (first l))
(defn end-segment [l] (second l))
(defn make-point [x y] [x y])
(defn x-point [p] (first p))
(defn y-point [p] (second p))
(defn print-point [p]
	(print "(")
	(print (x-point p))
	(print ",")
	(print (y-point p))
	(print ")")
	(println))
(defn midpoint-segment [l]
	(make-point (/ (+ (x-point (start-segment l))
                          (x-point (end-segment l)))
                       2)
                    (/ (+ (y-point (start-segment l))
                          (y-point (end-segment l)))
                       2)))

;2.3 decided to make this one for any parallelogram rather 
;	than just for rectangles. It's simpler and a bit more general.
; 	If you need to know whether a parallelogram is a rectangle,
;	we can make something to do that. Otherwise, this'll do.
(defn segment-length [l]
	(sqrt (+ 
		(square (- 
			(x-point (end-segment l))
			(x-point (start-segment l))))
		(square (-
			(y-point (end-segment l))
			(y-point (start-segment l)))))))
(defn rectangle-length [rectangle]
	(defn side1 []
		(make-segment (first rectangle) (second rectangle)))
	(defn side2 []
		(make-segment (second rectangle) (nth rectangle 2)))
	(max (segment-length (side1))
		(segment-length (side2))))
(defn rectangle-width [rectangle]
	(defn side1 []
		(make-segment (first rectangle) (second rectangle)))
	(defn side2 []
		(make-segment (second rectangle) (nth rectangle 2)))
	(min (segment-length (side1))
		(segment-length (side2))))
(defn rectangle-diagonal-length [rectangle]
	(segment-length (make-segment (first rectangle) (nth rectangle 2))))
(defn check-rectangle [rectangle]
	(if (< (abs (+ 
			(square (rectangle-length rectangle))
			(square (rectangle-width rectangle))
			(- (square (rectangle-diagonal-length rectangle))))) 0.1)
	true
	false))
(defn make-rectangle [a b c]
	(if 
		(check-rectangle [a b c])
		[a b c]
	(throw (Exception. "Please input three consecutive points of the 
	 rectangle."))))




(defn rectangle-area [rectangle]
	(* (rectangle-length rectangle) (rectangle-width rectangle)))
(defn rectangle-perimeter [rectangle]
	(* 2 (+ (rectangle-length rectangle) (rectangle-width rectangle))))






(defn hey-there [x y]  
  (defn dispatch [m]  
    (cond (= m 0) x  
          (= m 1) y  
          :else (throw (Exception. "Argument not 0 or 1" ))))  
  dispatch)


;2.4 Not shown


;2.5
(defn vect [a b]
 (* (expt 2 a) (expt 3 b)))
(defn first-vect [v]
 (defn iter [w c]
  	(if (= (rem w 2) 1)
   			c
   			(iter (/ w 2) (inc c))))
 	(iter v 0))
(defn second-vect [v]
 (defn iter [w c]
  	(if (not (= (rem w 3) 0))
   			c
   			(iter (/ w 3) (inc c))))
 	(iter v 0))


;2.6
(def zero (fn [f] (fn [x] x)))
(defn add-1 [n]
 	(fn [f] (fn [x] (f ((n f) x)))))
(def one (fn [f] (fn [x] (f x))))
(def two 
 	(fn [f] (fn [x] (f (f x)))))
(defn add-numbers [a b] ;(would take the place of defining +1)
 (fn [f] (fn [x] ((compose (a f) (b f)) x))))

;2.7
(defn make-interval [a b]
 [a b])
(defn upper-bound [v]
 (second v))
(defn lower-bound [v]
 (first v))

;2.8
(defn sub-interval [v w]
 (make-interval 
  (- (lower-bound v) (lower-bound w))
  (- (upper-bound v) (upper-bound w))))

;2.9 
; (mul-interval [2 4] [4 6]) = [8 24]. The widths of the arguments are
; both 1, but the width of the result is 16. So, if the width of the
; result is to be a function of only the widths of the arguments,
; we know (f 1 1)=16. But, then we note,
; (f 1 1) = (width (mul-interval [0 2] [0 2])) = (width [0 4]) = 2
; So, our assumption lead us to, 2 = (f 1 1) = 16 and since
; 2 does not equal 16, we know the width of the product is not a function
; of the widths of the arguments alone.


;2.10
(defn mul-interval [x y]
 (cond  (>= (lower-bound x) 0)
  	  (cond (>= (lower-bound y) 0)
  	         (make-interval (* (lower-bound x) (lower-bound y))
  				(* (upper-bound x) (upper-bound y)))
  		(<= (upper-bound y) 0)
  		 (make-interval (* (upper-bound x) (lower-bound y))
  		                (* (lower-bound x) (upper-bound y)))
  		:else (make-interval 
      		 	     	(* (upper-bound x) (lower-bound y))
  				(* (upper-bound x) (upper-bound y))))
        (<= (upper-bound x) 0)
  	  (cond (>= (lower-bound y) 0)
  		 (make-interval (* (lower-bound x) (upper-bound y))
  			        (* (upper-bound x) (lower-bound y)))
  		(<= (upper-bound y) 0)
  		 (make-interval (* (upper-bound x) (upper-bound y))
  			        (* (lower-bound x) (lower-bound y)))
  		:else (make-interval 
  			        (* (lower-bound x) (upper-bound y))
  				(* (lower-bound x) (lower-bound y))))
 :else 		(cond (>= (lower-bound y) 0)
  		 (make-interval (* (lower-bound x) (upper-bound y))
  				(* (upper-bound x) (upper-bound y)))
  		(<= (upper-bound y) 0)
  		 (make-interval (* (upper-bound x) (lower-bound y))
  				(* (lower-bound x) (lower-bound y)))
  		:else (let [a (* (lower-bound x) (upper-bound y))
  			    b (* (lower-bound y) (upper-bound x))]
  			(make-interval a b)))))

(defn interval-span-0? [a]
 (<= (* (upper-bound a) (lower-bound a)) 0))
(defn div-interval [x y]
 (if (interval-span-0? y)
  (throw (Exception. "We are not Chuck Norris. Try again."))
  (mul-interval x 
   (make-interval 
    (/ 1.0 (upper-bound y))
    (/ 1.0 (lower-bound y))))))


;2.12
(defn make-center-percent [center percent]
 (def right-endpoint 
  (* center (+ 1 (/ percent 100))))
 (def left-endpoint
  (* center (- 1 (/ percent 100))))
 (make-interval left-endpoint right-endpoint))

(defn center [interval]
 (/ (+ (upper-bound interval) (lower-bound interval)) 2))
(defn width [interval]
 (/ (- (upper-bound interval) (lower-bound interval)) 2))
(defn percent [interval]
 (* (/ (width interval) (center interval)) 100))


;2.13
(comment let sig1 and sig2 << 1. Then, 
 (a pm sig1)(b pm sig 2)=ab pm (a sig2) pm (b sig1) + sig1*sig2
 	  approximately = ab pm (a sig2 + b sig1) 
 	  So, the error in the product would be,
 	  (+ (* (center a) (/ (percent b) 100))
 	     (* (center b) (/ (percent a) 100))))



;2.14
;Rounding errors, and in the top one (as 2.15 will note), there are repeated variables. These repeated variables cause a miscalculation because at no point will any interval be divided by itself. For instance just for the purposes of this example, let R_1= (0,4) and R_2= (6,8). Then in the first formula, the number (4*8)/(0+6)=16/3 is within the interval of values, but this number would was achieved by letting the R_1 value on top be different from the R_1 value on the bottom (and the same with R_2). So, the fewer instances of any single variable the better.


;2.15 See 2.14


;2.16
; We can circumvent this issue to some degree of accuracy (not perfect, in all cases, sadly), by using the extreme value theorem. Basically, we know that given a continuous function f(x_1,...,x_n) from a product of closed intervals to the reals will have both a maximum and a minimum within said domain. Furthermore, we know that this function will achieve every value between said maximum and minimum due to the intermediate value theorem. So, our only task now is to find said max and min, which we can do by the method of bisection, but again, it would be an approximation.

;2.17
(defn last-pair [thing]
  (if (= (list ) (rest thing))
      thing
      (last-pair (rest thing))))
; In scheme this would be done by (if (= (cdr thing) nil) thing (last-pair (cdr thing)))




;2.18
(defn reverse-list [thing]
  (if (= (list ) (rest thing))
      (list (first thing))
      (concat (reverse-list (rest thing)) (list (first thing)))))



;2.19
(defn cc [amount coin-values]
  (defn no-more? [things] (= things (list )))
  (defn first-denomination [things] (first things))
  (defn except-first-denomination [things] (rest things))
  (cond (= amount 0) 1
        (or (< amount 0) (no-more? coin-values)) 0
        :else (+ (cc amount (except-first-denomination coin-values))
                 (cc (- amount (first-denomination coin-values))
                     coin-values))))
(def us-coins (list 50 25 10 5 1))
(def uk-coins (list 100 50 20 10 5 2 1 0.5))
;It does matter. If the lists were reversed there would be over-counting.
; For instance, let's assume that the lists were reversed, (cc 7 us-coins)
; would count for the case where we have one nickel and two pennies three times.


;2.20
(defn same-parity [x & y]
  (def parity (mod x 2))
  (defn find-parity [so-far left-over]
    (cond  (= left-over (list )) 
              so-far
           (= (mod (first left-over) 2) parity) 
              (find-parity (concat so-far (list (first left-over))) (rest left-over))
           :else (find-parity so-far (rest left-over))))
  (find-parity (list x) y))





;2.21
(defn square-list-1 [items]
  (if (= items (list )) nil
      (cons (square (first items)) (square-list-1 (rest items)))))
(defn square-list-2 [items]
  (map square items))




;2.22
;because every new item from the "items" list is added in the leftmost position in the
; answer list. So the order is reversed.
;In the second way, she doesn't have nil in the last position.


;2.23


(defn for-each [function items]
  (if (= (first (rest items)) nil) (function (first items))
      (do (function (first items)) (for-each function (rest items)))))


;2.24
; (cons 1 (cons 2 (cons 3 (cons 4 nil))))

;2.25
; cdrs => cdrs => cars => cdrs
; cars => cars
; cdrs => cdrs => cdrs => cdrs => cdrs => cdrs

;2.26
; (list 1 2 3 4 5 6)= (cons 1 (cons 2 ... ))
; ((list 1 2 3) 4 5 6)
; ((list 1 2 3) (cons (cons 4 (cons 5 (cons 6 nil))) nil))

;2.27
(defn deep-reverse [items]
  (cond (and (seq? items) (not (null? (rest items))))
          (list (deep-reverse (rest items)) (deep-reverse (first items)))
        (and (seq? items) (null? (rest items)))
          (deep-reverse (first items))
        :else items))

