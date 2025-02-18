;; http://www.generative-gestaltung.de/2/sketches/?02_M/M_6_1_02
(ns generative-design.spring
  (:require [p5]))


(def els #js {:a #js {} :b #js {}})
(def base-len 100)

(defn distance
  [^Node this ^Node that]
    (.dist (.-p this) (.-p that)))

(defn diff
  [^Node this ^Node that]
    (.sub p5/Vector (.-p this) (.-p that)))

(defn make-node [x y]
  #js {:p (js/createVector x y)})

(defn init-nodes []
    (set! (.-a els) (make-node 100 100))
    (set! (.-b els) (make-node 300 300)))

(defn update-particles
  [dist]
  (let [tension (* (- 0.01) (- dist base-len))
        direction (.normalize (diff (.-a els) (.-b els)))]

    (set! (.. els -a -p -x) (+ (.. els -a -p -x) (* tension 10 (.-x direction))))
    (set! (.. els -a -p -y) (+ (.. els -a -p -y) (* tension 10 (.-y direction))))

    (set! (.. els -b -p -x) (- (.. els -b -p -x) (* tension 10 (.-x direction))))
    (set! (.. els -b -p -y) (- (.. els -b -p -y) (* tension 10 (.-y direction))))))

(defn setup []
  (let [canvas (js/createCanvas js/windowWidth js/windowHeight)]
    (.parent canvas "p5jsparent")
    (js/fill 150)
    (js/stroke 100)
    (init-nodes)))

(defn draw
  []
  (js/background 255)
  (when js/mouseIsPressed
    (set! (.. els -a -p -x) js/mouseX)
    (set! (.. els -a -p -y) js/mouseY))
  (js/ellipse (.. els -a -p -x) (.. els -a -p -y) 10)
  (js/ellipse (.. els -b -p -x) (.. els -b -p -y) 10)
  (js/line (.. els -a -p -x) (.. els -a -p -y) (.. els -b -p -x) (.. els -b -p -y))
  (let [d (distance (.-a els) (.-b els))]
    (when (> d base-len)
      (update-particles d))))
