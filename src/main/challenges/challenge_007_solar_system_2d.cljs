(ns challenges.challenge-007-solar-system-2d
  (:require [p5])
  (:require-macros [helpers.macros :refer [half]]))

;; Estructuras de datos
(defn make-planet
  ([r d th orbit-speed & {:keys [n-moons level] :or {n-moons 0 level 1}}]
   {:r r
    :d d
    :th th
    :orbit-speed orbit-speed
    :ps
    (mapv
     (fn [_]
       (let [new-level (inc level)
             r (/ (+ (rand-int 50) 5) new-level)
             d (/ (+ (rand-int 100) 100) new-level)
             th (rand-int 360)
             os (rand-int 10)
             n-moons (rand-int 4)]
         (when (< level 3)
           (make-planet
            r d th os
            {:n-moons n-moons :level new-level}))))
     (range n-moons))}))

;; Estado
(def state {:sizes
            {:w 600 :h 600}})

(def width (-> state :sizes :w))
(def height (-> state :sizes :h))
(def sun (make-planet 100 0 0 0 {:level 0 :n-moons 5}))

;; Funciones
(defn show-planet
  [p]
  (js/push)
  (js/fill 150 100)
  (js/rotate (-> p :th (+ (* (/ (:orbit-speed p) 5) js/frameCount))))
  (js/translate (:d p) 0)
  (js/circle 0 0 (:r p))
  (doseq [sat (:ps p)]
    (show-planet sat))
  (js/pop))

;; P5js
(defn setup []
  (js/createCanvas width height)
  (js/angleMode js/DEGREES))

(defn draw []
  (js/background 0)
  (js/translate (half width) (half height))
  (js/stroke 255)
  (js/fill 150)

  (doseq [p (:ps sun)]
    (show-planet p)))

