(ns core.vector)

(defn make-random-3d-vector
  []
  [(rand) (rand) (rand)])

(defn make-vector
  [len init-value]
  (mapv (fn [_] init-value) (range len)))

(defmacro x-hat []
  [0 1 1])

(defmacro y-hat []
  [1 0 1])

(defmacro z-hat []
  [1 1 0])

(defmacro x-comp [v]
  `(first ~v))

(defmacro y-comp [v]
  `(second ~v))

(defmacro z-comp [v]
  `(nth ~v 2))

(defn vector-sum [& vs]
  (apply mapv + vs))

(defn- vector-reduce-sum [& vs]
  (reduce #(+ %1 %2) 0 (apply vector-sum vs)))

(defn- element-wise-operation [op v1 v2]
  (vec (map #(op %1 %2) v1 v2)))

(defn- constant-element-operation [op v m]
  (vec (map #(op % m) v)))

;; Slightly faster than using let + deconstructing
(defn add [v1 v2]
  (element-wise-operation + v1 v2))

(defn substract [v1 v2]
  (element-wise-operation - v1 v2))

(defn vector-dot [v1 v2]
  (element-wise-operation * v1 v2))

(defn dot [v1 v2]
  (vector-reduce-sum (vector-dot v1 v2)))

(defn scale-up [v m]
  (constant-element-operation * v m))

(defn scale-down [v m]
  (constant-element-operation / v m))

(defn negate [v]
  (into [] (map #(- %) v)))

(defn cross [v1 v2]
  [(- (* (second v1) (nth v2 2))
      (* (nth v1 2) (second v2)))
   (- (* (nth v1 2) (first v2))
      (* (first v1) (nth v2 2)))
   (- (* (first v1) (second v2))
      (* (second v1) (first v2)))])

(defn derivative
  "`vn` is a vector function."
  [eps vn t]
  (scale-down
   (substract (vn (+ t (/ eps 2))) (vn (- t (/ eps 2))))
   eps))

(defn sum-of-squares [v]
  (dot v v))

(defn magnitude [v]
  (Math/sqrt (sum-of-squares v)))

(defn distance [v w]
  (magnitude (substract v w)))

(defn normalize [v]
  (scale-up (/ 1 (magnitude v)) v))

(defn direction [v]
  (Math/atan2 (nth v 1) (nth v 0)))

(defn ->polar [v]
  {:direction (direction v)
   :magnitude (magnitude v)})

(defn ->xy [{:keys [direction magnitude]}]
  [(* (Math/cos direction) magnitude)
   (* (Math/sin direction) magnitude)])

(defn lerp [a b t]
  (+ a (* (- b a) t)))

(defn lerp2d [A B t]
  [(lerp (first A) (first B) t)
   (lerp (second A) (second B) t)])

(defn hat [v]
  (scale-down v (magnitude v)))


(comment
  (add [1 2 3] [4 5 6])
  (substract [1 2 3] [4 5 6])
  (element-wise-operation * [1 2 3] [4 5 6])
  (scale-down [10 15 -5] 5)
  (scale-up [10 15 -5] -2.1)
  (negate [1 2 3])
  (dot [1 2 3] [1 2 3])
  (cross [1 2 3] [4 5 6])
  (defn v1 [t] [5 (* 3 t) 0])
  (derivative 0.1 v1 1)
  (apply Math/hypot (hat [1 2 3]))
  )
