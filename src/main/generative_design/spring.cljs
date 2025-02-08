;; http://www.generative-gestaltung.de/2/sketches/?02_M/M_6_1_02
(ns generative-design.spring
  (:require [p5]))

(def els #js {:a #js {} :b #js {}})
(def base-len 100)
(def update-a (atom true))

(defn mouse-clicked
  []
  (set! (.. els -a -x) js/mouseX)
  (set! (.. els -a -y) js/mouseY))

(defn mouse-dragged
  []
  (set! (.. els -a -x) js/mouseX)
  (set! (.. els -a -y) js/mouseY)
  (reset! update-a false))

(defn mouse-released
  []
  (reset! update-a true))

(defn update-particles
  [dist]
  (let [tension (* (- 0.01) (- dist base-len))
        direction (.normalize (.sub p5/Vector (.-a els) (.-b els)))]
    (when @update-a
      (set! (.. els -a -x) (+ (.. els -a -x) (* tension 10 (.-x direction))))
      (set! (.. els -a -y) (+ (.. els -a -y) (* tension 10 (.-y direction)))))

    (set! (.. els -b -x) (- (.. els -b -x) (* tension 10 (.-x direction))))
    (set! (.. els -b -y) (- (.. els -b -y) (* tension 10 (.-y direction))))))

(defn setup []
  (let [canvas (js/createCanvas js/windowWidth js/windowHeight)]
    (.parent canvas "p5jsparent")
    (js/fill 150)
    (js/stroke 100)
    (set! (.-a els) (js/createVector 100 100))
    (set! (.-b els) (js/createVector 300 300))
    ))

(defn draw
  []
  (js/background 255)
  (js/ellipse (.. els -a -x) (.. els -a -y) 10)
  (js/ellipse (.. els -b -x) (.. els -b -y) 10)
  (js/line (.. els -a -x) (.. els -a -y) (.. els -b -x) (.. els -b -y))
  (let [d (.dist (.-a els) (.-b els))]
    (when (> d base-len)
      (update-particles d)))
  )
