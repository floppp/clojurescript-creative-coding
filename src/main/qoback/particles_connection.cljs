(ns qoback.particles-connection
  (:require [p5]))

(defn make-particle [idx]
  #js{:id idx
      :diameter 6
      :radius 3
      :pos #js {:x (js/random 0 400)
                :y (js/random 0 400)}
      :vel #js {:x (js/random -10 10)
                :y (js/random -10 10)}})

(def ps #js [])

(def dt 0.1)

(defn setup []
  (doseq [idx (range 20)]
    (aset ps idx (make-particle idx)))
  (let [canvas (js/createCanvas 800 600)]
    (.parent canvas "p5jsparent")
    (js/angleMode js/DEGREES)
    (js/background 30)))

(defn update-ps []
  (doseq [^js p ps]
    (let [new-x (+ (.. p -pos -x) (* dt (.. p -vel -x)))
          new-y (+ (.. p -pos -y) (* dt (.. p -vel -y)))]
      (set! (.. p -pos -x) new-x)
      (set! (.. p -pos -y) new-y)
      (when (< (.-x (.-pos p)) (.-radius p))
        (set! (.-x (.-pos p))  (.-radius p))
        (set! (.-x (.-vel p)) (- (.-x (.-vel p)))))
      (when (> (.-x (.-pos p)) (- 800 (.-radius p)))
        (set! (.-x (.-pos p)) (- 800 (.-radius p)))
        (set! (.-x (.-vel p)) (- (.-x (.-vel p)))))
      (when (< (.-y (.-pos p)) (.-radius p))
        (set! (.-y (.-pos p)) (.-radius p))
        (set! (.-y (.-vel p)) (- (.-y (.-vel p)))))
      (when (> (.-y (.-pos p)) (- 600 (.-radius p)))
        (set! (.-y (.-pos p)) (- 600 (.-radius p)))
        (set! (.-y (.-vel p)) (- (.-y (.-vel p))))))))

(def limit 200)

(defn compute-lines []
  ;; naive primero.
  (doseq [^js p ps
          ^js q ps]
    (let [d (Math/sqrt (+ (Math/pow (- (.. p -pos -x) (.. q -pos -x)) 2)
                          (Math/pow (- (.. p -pos -y) (.. q -pos -y)) 2)))]
      (when (and (not= 0 d) (< d limit))
        (js/stroke 255
                   255
                   255
                   (* (/ (- limit d) limit) 255))
        (js/line (.. p -pos -x)
                 (.. p -pos -y)
                 (.. q -pos -x)
                 (.. q -pos -y))))))

(defn draw []
  (js/background 30)
  (js/fill 50 255 255)
  (doseq [p ps]
    (js/circle (.. p -pos -x) (.. p -pos -y) (.-diameter p)))
  (update-ps)
  (compute-lines))
