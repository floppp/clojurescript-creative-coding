(ns colorful-coding.project-9-flow-field
  (:require [p5]))

(def points #js [])
(def mult 0.005)
(def ^:dynamic *max* 0)

(defn setup []
  (let [canvas (js/createCanvas 1000 800)
        density 50
        space (/ js/width density)]
    (.parent canvas "p5jsparent")
    (js/background 30)
    (js/angleMode js/DEGREES)
    (js/noiseDetail 1)
    (doseq [x (range 0 js/width space)
            y (range 0 js/height space)]
      (.push points (js/createVector (+ x (js/random -10 10)) (+ y (js/random -10 10)))))
    (js/shuffle points true)))

(defn draw []
  (js/noStroke)
  (binding [*max* (if (< js/frameCount (count points))
                    js/frameCount
                    (count points))]
    #_(js/background 30) ;; Esto permite simular movimiento browniano.
    (doseq [idx (range *max*)]
      (let [p (aget points idx)
            angle (js/map (js/noise (* mult (.-x p)) (* mult (.-y p))) 0 1 0 720)
            r (js/map (.-x p) 0 js/width 50 255)
            g (js/map (.-y p) 0 js/height 50 255)
            b (js/map (.-x p) 0 js/width 255 50)]
        (js/fill r g b)
        (.add p (js/createVector (js/cos angle) (js/sin angle)))
        (js/ellipse (.-x p) (.-y p) 1)))))
