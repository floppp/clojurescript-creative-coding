(ns challenges.challenge-102-2d-water-ripple
  (:require [p5]))


(def side 200)
(def cols side)
(def rows side)
(def size (* side side))
(def dampling 0.99)

;; (def current (volatile! (.fill (js/Array side)  (.fill (js/Array side) 0))))
;; (def previous (volatile! (.fill (js/Array side) (.fill (js/Array side) 0))))
;; (def temp (volatile! (.fill (js/Array side) (.fill (js/Array side) 0))))
(def current (volatile! (clj->js (mapv (fn [_] (mapv (constantly 0) (range side))) (range side)))))
(def previous (volatile! (clj->js (mapv (fn [_] (mapv (constantly 0) (range side))) (range side)))))
(def temp (volatile! (clj->js (mapv (fn [_] (mapv (constantly 0) (range side))) (range side)))))


(defn color->red [c]
  (aget (.-levels (js/color c)) 0))

(defn color->green [c]
  (aget (.-levels (js/color c)) 1))

(defn color->blue [c]
  (aget (.-levels (js/color c)) 2))

(defn color->alpha [c]
  (aget (.-levels (js/color c)) 3))


(defn mouse-dragged []
  ;; (js/loadPixels)
  ;; (println js/mouseX js/mouseY)
  (aset @previous js/mouseX js/mouseY 2550)
  ;; (println js/mouseX js/mouseY (* 4 (+ js/mouseX (* js/mouseY cols))))
  ;; (println (aget js/pixels 10000))
  ;; (js/updatePixels)
  ;; (println @previous)
  )

(defn setup []
  (js/pixelDensity 1)
  (js/createCanvas cols rows)
  (js/loadPixels)
  ;; (aset js/pixels 1000 199)
  )

(defn draw []
  (js/background 0)
  (js/loadPixels)

  (doseq [i (range 1 (dec cols))
          j (range 1 (dec rows))]
    (let [index (* 4 (+ i (* j cols)))
          n-value (* dampling
                     (-
                      (/
                       (+
                        (aget @previous (inc i) j)
                        (aget @previous (dec i) j)
                        (aget @previous i (inc j))
                        (aget @previous i (dec j)))
                       2)
                      (aget @current i j)))]
      ;; (when (> n-value 0)
      ;; (println index))
      #_(when (not= (aget @current i j) (aget @previous i j))
          (println i j index (aget @current i j) (aget @previous i j)))
      (aset @current i j n-value)
      (aset js/pixels index       n-value)
      (aset js/pixels (+ index 1) n-value)
      (aset js/pixels (+ index 2) n-value)
      ))

  (js/updatePixels)

  (vreset! temp @previous)
  (vreset! previous @current)
  (vreset! current @temp)
  )

(comment
  (for [i (range 10)
        j (range 10)]
    (let [f (* i j)]
      [f i j]))
  )
