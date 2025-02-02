(ns generative-design.colors
  (:require [goog.object :as g]
            [p5]))


(def image (atom nil))
(def colors #js [])
(def sort-mode (atom nil))

(defn preload []
  (reset! image (js/loadImage "/static/imagen.png")))

(defn setup []
  (let [canvas (js/createCanvas 800 600)]
    (.parent canvas "p5jsparent")
    (js/background 30)))

(def ^:dynamic i 0)

(defn draw []
  (let [tile-count (Math/floor (/ js/width (js/max js/mouseX 5)))
        rect-size (/ js/width tile-count)]
    (set! (.-length colors) 0)
    (.loadPixels @image)
    (doseq [grid-x (range 0 tile-count)
            grid-y (range 0 tile-count)]
      (let [px (int (* grid-x rect-size))
            py (int (* grid-y rect-size))
            i (* 4 (+ px (* py (.-width @image))))
            c (js/color (aget (.-pixels @image) i)
                        (aget (.-pixels @image) (inc i))
                        (aget (.-pixels @image) (+ i 2))
                        (aget (.-pixels @image) (+ i 3)))]
        (.push colors c)))
    (.sortColors js/gd colors @sort-mode)
    (doseq [grid-x (range 0 tile-count)
            grid-y (range 0 tile-count)]
      (binding [i (inc i)]
        (println i)
        (js/fill (aget colors i))
        (js/rect (* grid-x rect-size)
                 (* grid-y rect-size)
                 rect-size
                 rect-size)))
    )
  )

