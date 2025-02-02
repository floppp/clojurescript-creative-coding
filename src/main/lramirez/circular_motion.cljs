(ns lramirez.circular-motion
  (:require [p5]))

(defn duplicate []
  (let [size 40
        count 20]
    (js/translate 20 20)
    (doseq [x (range count)
            y (range count)]
      (js/push)
      (js/translate (* x size) (* y size))
      (js/fill 255 250 255 20)
      (js/stroke 255 10)
      (js/rotate (+ (* js/frameCount 0.05) (/ (+ x y) 3)))
      (js/ellipse 0 0 size size)
      (js/stroke 255)
      (js/strokeWeight 3)
      (js/point (/ size 3) (/ size 3) 1 1)
      (js/stroke 255 150)
      (js/strokeWeight 0.5)
      (js/line (/ size 3) (/ size 3) 1 1)
      (js/pop))))

(defn setup []
  (let [canvas (js/createCanvas 800 760)]
    (.parent canvas "p5jsparent"))
  )

(defn draw []
  (js/background 0)
  (duplicate))
