;; NO ESTÁ ACABADO
(ns lramirez.flow-field-generator
  (:require [p5]))

(def scl 20)
(def incr 0.1)
(def cols (volatile! nil))
(def rows (volatile! nil))
(def zoff (volatile! 0))
(def flow-field #js [])
(def ps #js [])

(defn make-particle []
  #js {:pos (js/createVector (js/random js/width) (js/random js/height))
       :vel (js/createVector 0 0)
       :acc (js/createVector 0 0)})

(defn setup []
  (js/createCanvas js/windowWidth js/windowHeight)
  (vreset! cols (js/floor (/ js/width scl)))
  (vreset! rows (js/floor (/ js/height scl)))

  (doseq [_ (range (* @cols @rows))]
    (.push flow-field 0))

  (doseq [_ (range 100)]
    (.push ps (make-particle))))


(defn update-particle
  [p]
  (.add ^js (.-vel p) ^js (.-acc p))
  (.add (.-pos p) ^js (.-vel p))
  (.mult ^js (.-acc p) 0))

(defn apply-force-to-particle
  [p f]
  (.add ^js (.-acc p) f))

(defn show-particle
  [p]
  (js/strokeWeight 4)
  (js/stroke 2)
  (js/point (.-x (.-pos p)) (.-y (.-pos p))))

(defn particle-in-edges [p]
  (when (> (.-x (.-pos p)) js/width)
    (set! (.-x (.-pos p)) 0))
  (when (< (.-x (.-pos p)) 0)
    (set! (.-x (.-pos p)) js/width))
  (when (< (.-y (.-pos p)) 0)
    (set! (.-y (.-pos p)) js/height))
  (when (> (.-y (.-pos p)) js/height)
    (set! (.-y (.-pos p)) 0)))

(defn particle-follows [p vs scl cols]
  (let [x (js/floor (/ (.-x (.-pos p)) scl))
        y (js/floor (/ (.-y (.-pos p)) scl))
        idx (+ x (* y cols))
        force (aget vs idx)]
    (apply-force-to-particle p force)))

(defn draw []
  (js/background 255)
  (js/stroke 255)
  (js/noFill)
  (js/beginShape)

  (doseq [y (range @rows)]
    (doseq [x (range @cols)]
      (let [idx (+ x (* y js/width))
            angle (* (js/noise (* incr x) (* incr y) @zoff) js/TWO_PI)
            v (p5/Vector.fromAngle angle)]
        (.setMag v 0.1)
        (aset flow-field idx v)
        (js/stroke 0)
        (js/push)
        (js/translate (* x scl) (* y scl))
        (js/rotate angle)
        (js/strokeWeight 1)
        (js/line 0 0 scl 0)
        (js/pop)
        (vswap! zoff (fn [v] (+ v 0.00001))))))

  (doseq [p ps]
    (particle-follows p flow-field scl @cols)
    (show-particle p)
    (update-particle p)
    (particle-in-edges p)))
