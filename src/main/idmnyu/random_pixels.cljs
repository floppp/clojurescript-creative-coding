(ns idmnyu.random-pixels
  (:require [p5]))

(defn mouse-dragged []
  (js/loadPixels)
  (let [index (* 4 (+ js/mouseX (* js/mouseY js/width)))]
    (aset js/pixels index 0)
    (aset js/pixels (+ 1 index) 0)
    (aset js/pixels (+ 2 index) 0)
    ;; (println index)
    )
  (js/updatePixels))

(defn setup []
  (js/createCanvas 320 240)
  (js/pixelDensity 1))

(defn draw []
  (js/background 255)
  (js/loadPixels)

  (doseq [y (range js/height)
          x (range js/width)]
    (let [index (* 4 (+ x (* y js/width)))
          red (aget js/pixels index)]
      (if (not= red 0)
        (do
          (aset js/pixels index x)
          (aset js/pixels (+ index 1) (rand-int 255))
          (aset js/pixels (+ index 2) y)
          (aset js/pixels (+ index 3) 255))
        (println index red))))

  (js/updatePixels))
