;; http://www.generative-gestaltung.de/2/sketches/?01_P/P_2_1_3_01
(ns generative-design.crazy-circles
  (:require [goog.object :as g]
            [p5]))

(def initial-color 90)
(def tile-count 20)
(def width 720)
(def height 720)
(def tile-height (/ height tile-count))
(def tile-width (/ width tile-count))


(defn setup []
  (let [canvas (js/createCanvas width height)]
    (.parent canvas "p5jsparent")
    (js/background initial-color)
    (js/noFill)
    ;; (js/strokeWeight 2)
    (js/stroke "white")
    ;; (reset! act-stroke-cap js/ROUND)
    ))

(defn draw []
  (js/background initial-color)
  (let [circle-count (inc (/ js/mouseX 30))
        end-size (js/map js/mouseX
                         0
                         (js/max js/width js/mouseX)
                         (/ tile-height 2)
                         0)
        end-offset (js/map js/mouseY
                           0
                           (js/max height js/mouseY)
                           0
                           (- tile-width end-size)
                           2)]
    (doseq [grid-y (range 0 tile-count)
            grid-x (range 0 tile-count)]
      (js/push)
      (js/translate (* tile-height grid-x) (* tile-width grid-y))
      (js/scale 1 (/ tile-width tile-height))
      (let [toggle (int (js/random 0 4))]
        (cond
          (= 0 toggle) (js/rotate (- js/HALF_PI))
          (= 1 toggle) (js/rotate 0)
          (= 2 toggle) (js/rotate js/HALF_PI)
          (= 3 toggle) (js/rotate js/PI)))
      (doseq [i (range circle-count)]
        (let [diameter (js/map i 0 circle-count tile-width end-size)
              offset (js/map i 0 circle-count 0 end-offset)]
          (js/ellipse offset 0 diameter diameter)))
      (js/pop))))
