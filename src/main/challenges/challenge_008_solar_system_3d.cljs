(ns challenges.challenge-008-solar-system-3d
  (:require [p5]
            [core.vector :as v])
  (:require-macros [helpers.macros :refer [twice]]
                   [core.vector :refer [y-hat]]))
(defn make-planet
  ([p-radious d th orbit-speed & {:keys [n-moons level] :or {n-moons 0 level 1}}]
   {:r p-radious
    :d d
    :th th
    :orbit-speed orbit-speed
    :ps
    (mapv
     (fn [_]
       (let [new-level (inc level)
             r (/ p-radious (twice new-level))
             d (+ (rand-int (+ r p-radious)) (+ r p-radious))
             th (rand-int 360)
             os (inc (rand-int 5))
             n-moons 1 #_(rand-int 2)]
         (when (< level 2)
           (make-planet
            r d th os
            {:n-moons n-moons :level new-level}))))
     (range n-moons))
    :v (v/scale-up (v/make-random-3d-vector) d)}))

;; Estado
(def state {:sizes
            {:w 600 :h 600}})

(def width (-> state :sizes :w))
(def height (-> state :sizes :h))
(def sun (make-planet 50 0 0 0 {:level 0 :n-moons 3}))

;; Funciones
(defn show-planet
  [p]
  (js/push)
  (let [[x y z] (:v p)
        [x-rot y-rot z-rot] (v/cross (:v p) (y-hat))
        angle (*
               (/ (:orbit-speed p) 5)
               js/frameCount)
        ]
    (js/rotate angle (array x-rot y-rot z-rot))

    (js/stroke 255)
    (js/line 0 0 0 x y z)
    ;; (js/line 0 0 0 (* 5 x-rot) (* 5 y-rot) (* 5 z-rot))
    (js/translate x y z)
    (js/noStroke) ;; Para no dibujar la malla de las esferas.

    (js/fill 150 100)
    (js/sphere (-> p :r)))
  (doseq [sat (:ps p)]
    (show-planet sat))
  (js/pop))

;; P5js
(defn setup []
  (let [canvas (js/createCanvas width height js/WEBGL)]
    (.parent canvas "p5jsparent")
    (js/angleMode js/DEGREES)))

(defn draw []
  (js/background 0)
  (js/noStroke)
  (js/fill 150)
  (js/pointLight 255 255 255 0 0 0)
  (js/lights)
  (js/sphere (-> sun :r))

  (doseq [p (:ps sun)]
    (show-planet p))
  (js/orbitControl))
